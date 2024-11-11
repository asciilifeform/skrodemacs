;;  -*- lexical-binding: t; -*-

;;; skroad.el --- Experimental font-lockified version of skrode.el.
;;; (add-to-list 'auto-mode-alist '("\\.skroad\\'" . skroad-mode))
;;; After this is done, s/skroad/skrode.

;;; Utility functions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--debug nil)

(unless skroad--debug
  (setq byte-compile-warnings nil))

(defun skroad--keyword-to-symbol (exp)
  "If EXP is a keyword, convert it to a symbol. If not, return it as-is."
  (unless (keywordp exp)
    (error "%s is not a keyword!" exp))
  (read (substring (symbol-name exp) 1)))

(defmacro skroad--do-plist (key val plist &rest body)
  "Evaluate BODY for side-effects with key,val bound to each pair in PLIST."
  (declare (indent defun))
  (let ((l (gensym)))
    `(let ((,l ,plist))
       (while ,l
         (let ((,key (car ,l)) (,val (cadr ,l)))
           ,@body
           (setq ,l (cddr ,l)))))))

(defun skroad--get-start-of-line (pos)
  "Get the position of the start of the line on which POS resides."
  (save-mark-and-excursion
    (goto-char pos)
    (line-beginning-position)))

(defun skroad--get-end-of-line (pos)
  "Get the position of the end of the line on which POS resides."
  (save-mark-and-excursion
    (goto-char pos)
    (line-end-position)))

(defmacro skroad--with-whole-lines (start end &rest body)
  "Get expanded region defined by START and END that spans whole lines."
  (declare (indent defun))
  `(let ((start-expanded (skroad--get-start-of-line ,start))
         (end-expanded (skroad--get-end-of-line ,end)))
     ,@body))

(defun skroad--overlay-active-p (overlay)
  "Determine whether OVERLAY is currently active."
  (and (overlayp overlay)
       (eq (current-buffer) (overlay-buffer overlay))))

(defmacro skroad--silence-modifications (function)
  "Prevent FUNCTION from triggering modification hooks while in this mode."
  `(advice-add ,function :around
               (lambda (orig-fun &rest args)
                 (if (eq major-mode 'skroad-mode)
                     (with-silent-modifications
                       (apply orig-fun args))
                   (apply orig-fun args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--floating-title-enable t
  "Display floating title at the top of the window if title is not in view.")

(defface skroad--text-face '((t :inherit default))
  "Default face used for skrode text types."
  :group 'skroad-faces)

(defface skroad--selector-face
  '((t :inherit highlight :extend t))
  "Face for use with atomic selections."
  :group 'skroad-faces)

(defface skroad--renamer-face
  '((t :inherit skroad--text-face
       :foreground "white" :background "ForestGreen"))
  "Face for use with renamer."
  :group 'skroad-faces)

(defface skroad--title-face
  '((t :inherit skroad--text-face
       :foreground "white" :background "purple"
       :height 300 :weight bold :extend t))
  "Face for skroad node titles."
  :group 'skroad-faces)

(defface skroad--title-renamer-face
  '((t :inherit skroad--title-face
       :foreground "white" :background "ForestGreen"))
  "Face for renamer in skroad node titles."
  :group 'skroad-faces)

(defface skroad--dead-link-face
  '((t :inherit link :foreground "red"))
  "Face used for dead links.."
  :group 'skroad-faces)

(defface skroad--heading-face
  '((t :inherit skroad--text-face
       :weight bold :height 1.2 :inverse-video t))
  "Face used for skroad heading text."
  :group 'skroad-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--type-fn (text-type name &rest args)
  "Evaluate function NAME, which must be defined for TEXT-TYPE, with ARGS."
  (let ((fn (get text-type name)))
    (unless fn
      (error "%s is not defined for type %s !" fn text-type))
    (apply fn text-type args)))

(defun skroad--define-text-type (name &rest properties)
  "Define a new skroad text type NAME with given PROPERTIES."
  (if (plist-get properties :mixin) ;; Save a mixin's properties verbatim.
      (setf (symbol-plist name) properties) ;; ...will be eaten when :use'd.
    (letrec ;; Otherwise, walk all properties, including from any :use'd types:
        ((env nil) ;; Lexical environment for eval to use
         (save-pv #'(lambda (prop val) ;; when we get final value of a prop,
                      (put name prop val) ;; record as a symbol-prop of name,
                      (push (cons prop val) env))) ;; and to where eval sees it
         (require-prop #'(lambda (prop) ;; Verify that prop was defined already
                           (when (null (alist-get prop env))
                             (error "%s must be defined for %s!" prop name))))
         (eat-props ;; Walk a plist, find and save effective value of each prop
          #'(lambda (plist)
              (skroad--do-plist prop val plist
                (cond
                 ((memq prop '(:doc :mixin)) nil) ;; Specials
                 ((eq prop :register) (add-to-list val name)) ;; Add to list
                 ((eq prop :use) ;; Val is another type; eat all of its props
                  (funcall eat-props (symbol-plist val))) ;; recursively
                 ((eq prop :defaults) ;; Default values for props when needed
                  (dolist (kv val)
                    (let ((def-prop (car kv)) (def-val (cadr kv)))
                      (when (null (alist-get def-prop env))
                        (funcall save-pv def-prop def-val)))))
                 ((eq prop :require) ;; val is mandatory prop (or list of them)
                  (if (listp val)
                      (dolist (p val) (funcall require-prop p))
                    (funcall require-prop val)))
                 ((keywordp prop) ;; Explicitly-defined property (:prop value)
                  (funcall
                   save-pv (skroad--keyword-to-symbol prop) ;; lose the `:`
                   (cond
                    ((eq prop :keymap) ;; If provided a keymap:
                     (let ((parent-keymap (alist-get 'keymap env)))
                       (when parent-keymap ;; stack on any existing keymap
                         (set-keymap-parent val parent-keymap))
                       val))
                    ((symbolp val) ;; symbols
                     (if (or (boundp val) (fboundp val) (facep val))
                         val (eval val env))) ;; if globally-bound, self-eval
                    (t (byte-compile (eval val env)))))) ;; compile form
                 ;; Property from :use, so don't re-eval it, just store it:
                 (t (unless (eq prop 'type-name) ;; Don't save :use'd type names
                      (funcall save-pv prop val))))))))
      (funcall save-pv 'type-name name) ;; Save the type name to symbol and env
      ;; Add properties, starting with the default ones:
      (funcall eat-props (append '(:use skroad--default-type) properties))))
  name)

(skroad--define-text-type
 'skroad--default-type
 :doc "Default text type from which all other types (except mixins) inherit."
 :mixin t
 :order 100 ;; lower number will get rendered first
 :face 'skroad--text-face
 :rear-nonsticky t)

(skroad--define-text-type
 'skroad--text-delimited
 :doc "Base mixin for delimited text types. Define delimiters before mixing."
 :mixin t
 :require 'payload-regex
 :defaults '((start-delim "") (end-delim "") (match-number 1))
 :make-text '(lambda (payload) (concat start-delim payload end-delim))
 :start-delim-regex
 '(if (string-empty-p start-delim) "" (concat (regexp-quote start-delim) "\s*"))
 :end-delim-regex
 '(if (string-empty-p end-delim) "" (concat "\s*" (regexp-quote end-delim)))
 :make-regex '(lambda (s) (concat start-delim-regex s end-delim-regex))
 :regex-any '(funcall make-regex payload-regex))

(skroad--define-text-type
 'skroad--text-findable
 :doc "Mixin for findable text types. (Internal use only.)"
 :mixin t
 :require '(make-regex regex-any finder-regex-forward make-text)
 :find-any-forward '(funcall finder-regex-forward regex-any)
 :find-any-backward '(funcall finder-regex-backward regex-any)
 :find-string-forward
 '(lambda (limit s)
    (funcall (funcall finder-regex-forward (funcall make-regex s)) limit))
 :find-string-backward
 '(lambda (limit s)
    (funcall (funcall finder-regex-backward (funcall make-regex s)) limit))
 :jump-next-from
 '(lambda (pos)
    (goto-char
     (or (save-mark-and-excursion
           (when (or (and (goto-char pos)
                          (funcall find-any-forward (point-max)))
                     (and (goto-char (point-min))
                          (funcall find-any-forward (point-max))))
             (match-beginning 0)))
         (point))))
 :jump-prev-from
 '(lambda (pos)
    (goto-char
     (or (save-mark-and-excursion
           (when (or (and (goto-char pos)
                          (funcall find-any-backward (point-min)))
                     (and (goto-char (point-max))
                          (funcall find-any-backward (point-min))))
             (match-beginning 0)))
         (point))))
 ;; :payload-range
 ;; '(lambda (start)
 ;;    (save-mark-and-excursion
 ;;      (goto-char start)
 ;;      (funcall find-any-forward (point-max))
 ;;      (list (match-beginning match-number) (match-end match-number))))
 :for-all-in-region-forward
 '(lambda (start end f)
    (save-mark-and-excursion
      (goto-char start)
      (while (funcall find-any-forward end)
        (funcall f (match-string-no-properties match-number)))))
 :replace-all
 '(lambda (s s-new)
    (save-mark-and-excursion
      (goto-char (point-min))
      (while (funcall find-string-forward (point-max) s)
        (replace-match s-new))))
 :replace-with-type
 '(lambda (s new-type)
    (funcall replace-all s (funcall (get new-type 'make-text) s))))

(defun skroad--finder-regex-forward (r)
  "Generate a forward finder for regex R."
  (lambda (limit) (re-search-forward r limit t)))

(defun skroad--finder-regex-backward (r)
  "Generate a backward finder for regex R."
  (lambda (limit) (re-search-forward r limit t)))

(skroad--define-text-type
 'skroad--text-delimited-anywhere
 :doc "Mixin for delimited text types found anywhere in the buffer."
 :mixin t
 :use 'skroad--text-delimited
 :finder-regex-forward #'skroad--finder-regex-forward
 :finder-regex-backward #'skroad--finder-regex-backward
 :use 'skroad--text-findable)

(defun skroad--in-title-p (&optional pos)
  "Return t if POS (default: point) is in the node title; otherwise nil."
  (save-mark-and-excursion
    (when pos (goto-char pos))
    (eq (line-beginning-position) (point-min))))

(defun skroad--body-start ()
  "Return the first position in the buffer outside of the node title."
  (save-mark-and-excursion
    (goto-char (point-min))
    (goto-char (line-beginning-position 2))
    (point)))

(defun skroad--finder-regex-forward-non-title (r)
  "Generate a forward finder for regex R which excludes the title."
  (lambda (limit)
    (when (skroad--in-title-p) (goto-char (skroad--body-start)))
    (let ((lim (if (< (point) (or limit (point-max)))
                   limit (line-end-position))))
      (re-search-forward r lim t))))

(defun skroad--finder-regex-backward-non-title (r)
  "Generate a backward finder for regex R which excludes the title."
  (lambda (limit)
    (let ((lim (if (skroad--in-title-p limit) (skroad--body-start) limit)))
      (unless (skroad--in-title-p) (re-search-backward r lim t)))))

(skroad--define-text-type
 'skroad--text-delimited-non-title
 :doc "Mixin for delimited text types excluded from the node title."
 :mixin t
 :use 'skroad--text-delimited
 :finder-regex-forward #'skroad--finder-regex-forward-non-title
 :finder-regex-backward #'skroad--finder-regex-backward-non-title
 :use 'skroad--text-findable)

(defvar skroad--rendered-text-types nil "Text types for use with font-lock.")

(skroad--define-text-type
 'skroad--text-rendered
 :doc "Finalization mixin for all text types rendered by font-lock."
 :mixin t
 :require '(find-any-forward render)
 :render-next
 '(lambda (limit)
    (when (funcall find-any-forward limit)
      (with-silent-modifications (funcall render) t)))
 :font-lock-rule '(lambda () (list render-next '(0 nil append)))
 :register 'skroad--rendered-text-types)

(defun skroad--init-font-lock ()
  "Initialize font-lock rules for a skroad mode buffer."
  (let ((rules nil)
        (types (sort skroad--rendered-text-types
                     #'(lambda (a b) (> (get a 'order) (get b 'order))))))
    (dolist (type types)
      (push (funcall (get type 'font-lock-rule)) rules))
    (font-lock-add-keywords nil rules t)))

(skroad--define-text-type
 'skroad--text-render-delimited-zoned
 :doc "Mixin for zoned delimited text types rendered by font-lock."
 :mixin t
 :require 'face
 :render
 '(lambda ()
    (set-text-properties
     (match-beginning 0) (match-end 0)
     (list 'category type-name
           'id (gensym)
           'face face
           'data (match-string-no-properties match-number))))
 :use 'skroad--text-rendered)

(skroad--define-text-type
 'skroad--text-render-delimited-decorative
 :doc "Mixin for decorative delimited text types rendered by font-lock."
 :mixin t
 :use 'skroad--text-delimited-anywhere
 :require 'face
 :render
 '(lambda () (add-face-text-property (match-beginning 0) (match-end 0) face))
 :order 1000
 :use 'skroad--text-rendered)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(skroad--define-text-type
 'skroad-decorative-italic
 :doc "Italicized text."
 :face 'italic
 :start-delim "__" :end-delim "__"
 :payload-regex "\\([^_]+\\)"
 :use 'skroad--text-render-delimited-decorative)

(skroad--define-text-type
 'skroad-decorative-bold
 :doc "Bold text."
 :face 'bold
 :start-delim "**" :end-delim "**"
 :payload-regex "\\([^*]+\\)"
 :use 'skroad--text-render-delimited-decorative)

(skroad--define-text-type
 'skroad-decorative-heading
 :doc "Heading text."
 :face 'skroad--heading-face
 :payload-regex "^##\s*\\([^\n\t\s]+[^\n\t]*\\)"
 :use 'skroad--text-render-delimited-decorative)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar skroad--indexed-text-types nil "Text types that are indexed.")

(skroad--define-text-type
 'skroad--text-indexed
 :doc "Finalization mixin for indexed text types."
 :mixin t
 :require 'for-all-in-region-forward
 :index-scan-region
 '(lambda (changes start end delta)
    (funcall
     for-all-in-region-forward start end
     #'(lambda (payload)
         (let* ((key (cons type-name payload))
                (count (+ delta (or (gethash key changes) 0)))) ;; inc/dec
           (if (zerop count) ;; if both added and removed since last update.
               (remhash key changes) ;; ...discard item from changes table.
             (puthash key count changes)))))) ;; otherwise, update.
 :register 'skroad--indexed-text-types)

(defun skroad--index-scan-region (changes start end delta)
  "Apply DELTA (must be 1 or -1) to each indexed item found in START..END,
updating the hash table CHANGES, and `skroad--index-update` must be called on
it to finalize all pending changes when no further ones are expected."
  (dolist (text-type skroad--indexed-text-types) ;; walk all indexed types
    (funcall (get text-type 'index-scan-region) changes start end delta)))

(defun skroad--index-update (index pending &optional init-scan)
  "Update INDEX by applying all PENDING changes, and run text type actions when
appropriate. If `INIT-SCAN` is t, run a text type's `on-init` rather than
`on-create` for created entries; `on-destroy` runs for destroyed ones."
  (maphash
   #'(lambda (key delta) ;; key and count delta in pending changes table
       (let* ((prior (or (gethash key index) 0)) ;; copies in index prior
              (create (zerop prior)) ;; t if index did not contain this item
              (count (+ prior delta)) ;; copies of item in index + delta
              (destroy (zerop count)) ;; t if change will destroy all copies
              (action ;; text type action to invoke, if any. nil if none.
               (cond (create (if init-scan 'on-init 'on-create))
                     (destroy (remhash key index) 'on-destroy))))
         (unless destroy (puthash key count index)) ;; update index if remains
         (let ((text-type (car key)) (payload (cdr key))) ;; args for action
           (skroad--type-action text-type action text-type payload))))
   pending)
  t)

(defvar-local skroad--index nil "Text type index for current buffer.")
(defvar-local skroad--changes nil "Pending index changes for current buffer.")

(defun skroad--init-local-index ()
  "Create the buffer-local indices and populate them from current buffer."
  (unless (null skroad--index)
    (error "Text type index already exists for this buffer!"))
  (setq skroad--index (make-hash-table :test 'equal))
  ;; Populate while dispatching `on-init`s
  (let ((init-populate (make-hash-table :test 'equal)))
    (skroad--index-scan-region init-populate (point-min) (point-max) 1)
    (skroad--index-update skroad--index init-populate t)))

(defun skroad--update-local-index ()
  "Apply all pending changes queued for the buffer-local text type index."
  (when skroad--changes
    (skroad--index-update skroad--index skroad--changes)
    (setq skroad--changes nil)))

(defun skroad--before-change-function (start end)
  "Triggers prior to a change in a skroad buffer in region START...END."
  (when (null skroad--changes)
    (setq skroad--changes (make-hash-table :test 'equal)))
  (skroad--with-whole-lines start end
    (skroad--index-scan-region
     skroad--changes start-expanded end-expanded -1)))

(defun skroad--after-change-function (start end length)
  "Triggers following a change in a skroad buffer in region START...END."
  (skroad--with-whole-lines start end
    (skroad--index-scan-region
     skroad--changes start-expanded end-expanded 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; (index final
    ;;   (let ((env))
    ;;     (skroad--do-plist p v (symbol-plist name) (push (cons p v) env))
    ;;     (dolist (f (reverse functions))
    ;;       (let* ((f-name (car f))
    ;;              (f-def (cadr f))
    ;;              (fn (read
    ;;                   (format "skroad--type-%s-%s"
    ;;                           (symbol-name name) (symbol-name f-name))))
    ;;              ;; (compiled
    ;;              ;;  (eval
    ;;              ;;   `(byte-compile (lambda ,(car f-def) ,@(cdr f-def))) env))
    ;;              (compiled
    ;;               (eval
    ;;                `(lambda ,(car f-def) ,@(cdr f-def)) env))
    ;;              )
    ;;         (message (format "fn=%s body=%s" f-name compiled))
    ;;         ;; (message (format "fn=%s body=%s" f-name f-def))
    ;;         (defalias fn compiled)
    ;;         (push (cons f-name compiled) env)
    ;;         (put name f-name fn)))
    ;;     (message (format "env=%s done" env))
    ;;     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--type-action (text-type action-name &rest args)
  "If ACTION-NAME is not nil, and TEXT-TYPE has a defined action of that name,
call the action with ARGS."
  (when action-name (let ((action (get text-type action-name)))
                      (when action (apply action args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--atomic-at (&optional pos)
  "Get the payload of the atomic found at the given POS or point, nil if none."
  (get-text-property (or pos (point)) 'data))

(defun skroad--type-at (&optional pos)
  "Determine text type, if any, at position POS (or point.)"
  (get-char-property (or pos (point)) 'category))

(defun skroad--kbd-doc-at (&optional pos)
  "Determine key doc, if any, at position POS (or point.)"
  (get-char-property (or pos (point)) 'kbd-doc))

(defun skroad--zone-at (&optional pos)
  "Return the zone ID at POS (or point)."
  (get-char-property (or pos (point)) 'id))

(defun skroad--zone-start (&optional pos)
  "Return the position at which the zone at POS starts."
  (or (previous-single-property-change (1+ (or pos (point))) 'id) (point-min)))

(defun skroad--zone-end (&optional pos)
  "Return the position at which the zone at POS ends."
  (or (next-single-property-change (or pos (point)) 'id) (point-max)))

(defmacro skroad--with-zone (&rest body)
  "Evaluate BODY with start and end bound to boundaries of zone at point."
  (declare (indent defun))
  `(let ((start (skroad--zone-start)) (end (skroad--zone-end)))
     ,@body))

;; TODO: abolish
(defmacro skroad--with-link-at-point (&rest body)
  "Evaluate BODY with link bound to the link under the point."
  `(let ((link (skroad--atomic-at (point))))
     (when link
       ,@body)))

(defun skroad--cmd-backspace ()
  "If prev point contains a link, delete the link. Otherwise backspace."
  (interactive)
  (let ((p (point)))
    (cond ((use-region-p) (delete-region (region-beginning) (region-end)))
          ((> p (skroad--body-start))
           (if (skroad--atomic-at (1- p))
               (delete-region (skroad--zone-start (1- p)) p)
             (delete-char -1))))))

(defun skroad--cmd-jump-to-next-link ()
  "Jump to the next link following point; cycle to first after the last."
  (interactive)
  (funcall (get 'skroad-live 'jump-next-from) (point)))

(defun skroad--cmd-jump-to-prev-link ()
  "Jump to the previous link preceding point; cycle to last after the first."
  (interactive)
  (funcall (get 'skroad-live 'jump-prev-from) (point)))

(defvar skroad--mode-keymap
  (define-keymap
    "<remap> <delete-backward-char>" #'skroad--cmd-backspace
    "<tab>" #'skroad--cmd-jump-to-next-link
    "C-<tab>" #'skroad--cmd-jump-to-prev-link)
  "Keymap for skroad mode.")

;;; Text Types. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst skroad--text-properties
  '(category face id data)
  "Properties added by font-lock that must be removed when unfontifying.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--cmd-atomics-prepend-space ()
  "Insert a space immediately behind the atomic currently under the point."
  (interactive)
  (save-mark-and-excursion
    (goto-char (skroad--zone-start))
    (insert " ")))

(defvar-local skroad--alt-mark nil
  "Opposite end of a link in which the mark had been set.")

(defun skroad--deactivate-mark ()
  "Deactivate the mark and clear the alt-mark."
  (deactivate-mark)
  (setq-local skroad--alt-mark nil))

(defmacro skroad--define-atomics-region-cmd (wrap-command)
  "Wrap COMMAND to use region if exists, or use the atomic at point as region."
  `(defun ,(read (concat "skroad--cmd-atomics-"
                         (symbol-name wrap-command))) ()
     (interactive)
     (if (use-region-p)
         (call-interactively ',wrap-command)
       (skroad--with-zone
         (funcall #',wrap-command start end)))
     (skroad--deactivate-mark)))

(skroad--define-atomics-region-cmd delete-region)
(skroad--define-atomics-region-cmd kill-region)
(skroad--define-atomics-region-cmd kill-ring-save)

(defvar-local skroad--selector nil
  "Selector overlay active when an atomic is under the point.")

(defconst skroad--selector-properties
  `((face skroad--selector-face) (evaporate t))
  "Text properties of the selector.")

(defun skroad--selector-show ()
  "Reveal the selector overlay when it may have been hidden."
  (when (skroad--overlay-active-p skroad--selector)
    (overlay-put skroad--selector 'face 'skroad--selector-face)))

(defun skroad--selector-hide ()
  "Hide (but not destroy) the selector overlay."
  (when (skroad--overlay-active-p skroad--selector)
    (overlay-put skroad--selector 'face nil)))

(defun skroad--selector-activate ()
  "Activate (if inactive) or move the selector to the current zone."
  (skroad--with-zone
    (move-overlay skroad--selector start end (current-buffer)))
  (setq-local cursor-type nil)
  (setq-local show-paren-mode nil))

(defun skroad--selector-deactivate ()
  "Deactivate the selector; it can be reactivated again."
  (when (skroad--overlay-active-p skroad--selector)
    (delete-overlay skroad--selector))
  (setq-local cursor-type t)
  (setq-local show-paren-mode t))

(defun skroad--cmd-atomics-set-mark ()
  "Set the mark inside an atomic."
  (interactive)
  (save-excursion
    (skroad--with-zone
      (setq-local skroad--alt-mark start)
      (goto-char end)
      (call-interactively 'set-mark-command))))

(defun skroad--cmd-atomics-jump-to-next-link ()
  "Jump to the next link following this atomic; cycle to first after the last."
  (interactive)
  (funcall (get 'skroad-live 'jump-next-from) (skroad--zone-end)))

(skroad--define-text-type
 'skroad-atomic
 :doc "Selected, clicked, killed, etc. as units. Point sits only on first pos."
 :on-enter '(lambda (pos-from auto)
              (skroad--selector-activate)
              (goto-char (skroad--zone-start))
              (let ((kbd-doc (skroad--kbd-doc-at)))
                (when kbd-doc (message kbd-doc))))
 :on-leave '(lambda (pos-from auto) (skroad--selector-deactivate))
 :on-move '(lambda (pos-from auto)
             (goto-char
              (if (> (point) pos-from)
                  (skroad--zone-end) (skroad--zone-start))))
 :keymap
 (define-keymap
   "<tab>" #'skroad--cmd-atomics-jump-to-next-link
   "SPC" #'skroad--cmd-atomics-prepend-space
   "<remap> <set-mark-command>" #'skroad--cmd-atomics-set-mark
   "<remap> <self-insert-command>" #'ignore
   "<deletechar>" #'skroad--cmd-atomics-delete-region
   "<backspace>" #'skroad--cmd-atomics-delete-region
   "<drag-mouse-1>" #'ignore "<drag-mouse-2>" #'ignore "<drag-mouse-3>" #'ignore
   "<down-mouse-1>" #'ignore "<down-mouse-2>" #'ignore "<down-mouse-3>" #'ignore
   "<mouse-1>" #'ignore "<mouse-2>" #'ignore "<mouse-3>" #'ignore
   "<remap> <kill-region>" #'skroad--cmd-atomics-kill-region
   "<remap> <kill-ring-save>" #'skroad--cmd-atomics-kill-ring-save
   )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--do-link-action (pos)
  "Run action of link at POS, if one was defined, and no region is active."
  (unless (use-region-p)
    (skroad--type-action
     (skroad--type-at pos) 'on-activate (skroad--atomic-at pos))))

(defun skroad--cmd-left-click-link (click)
  "Perform the action attribute of the link that got the CLICK."
  (interactive "e")
  (let ((estart (event-start click)))
    (select-window (posn-window estart))
    (skroad--do-link-action (posn-point estart))))

(defun skroad--cmd-enter-link ()
  "Perform the action attribute of the link at point."
  (interactive)
  (skroad--do-link-action (point)))

(skroad--define-text-type
 'skroad-link
 :doc "Fundamental type from which all skroad links are derived."
 :use 'skroad-atomic
 :face 'link
 :mouse-face 'highlight
 :keymap (define-keymap
           "<down-mouse-1>" #'skroad--cmd-left-click-link
           "RET" #'skroad--cmd-enter-link))

(defun skroad--link-to-plain-text ()
  "Delinkify the link under the point to plain text by removing delimiters."
  (interactive)
  (skroad--with-zone
    (let ((text (skroad--atomic-at)))
      (save-mark-and-excursion
        (goto-char start)
        (delete-region start end)
        (insert text)))))

;; TODO: preview linked node
(defun skroad--link-mouseover (window buf position)
  "User is mousing over a link in WINDOW, BUF, at POSITION."
  (with-current-buffer buf
    (skroad--atomic-at position)))

(skroad--define-text-type
 'skroad-node-link
 :doc "Fundamental type for skroad node links (live or dead)."
 :use 'skroad-link
 :help-echo 'skroad--link-mouseover
 :payload-regex "\\([^][\n\t\s]+[^][\n\t]*?\\)"
 :keymap (define-keymap
           "t" #'skroad--link-to-plain-text))

(defun skroad--live-link-to-dead ()
  "Transform all live links with payload LINK to dead links."
  (interactive)
  (skroad--with-link-at-point
   (funcall (get 'skroad-live 'replace-with-type) link 'skroad-dead)))

(defun skroad--browse-skroad-link (data)
  (message (format "Live link pushed: '%s'" data)))

(defun skroad--link-init (text-type payload)
  ;; (message (format "Link init: type=%s payload='%s'" text-type payload))
  )

(defun skroad--link-create (text-type payload)
  (message (format "Link create: type=%s payload='%s'" text-type payload))
  )

(defun skroad--link-destroy (text-type payload)
  (message (format "Link destroy: type=%s payload='%s'" text-type payload))
  )

(defun skroad--cmd-rename-remote-node ()
  "Activate the renamer for the current zone."
  (interactive)
  (skroad--with-zone
    (skroad--renamer-activate
     'skroad-node-remote-renamer start end)))

(skroad--define-text-type
 'skroad-live
 :doc "Live (i.e. navigable, and producing backlink) link to a skroad node."
 :kbd-doc "<return> go|<r> rename|<l> deaden|<t> textify|<del> delete|<spc> prepend space"
 :use 'skroad-node-link
 :on-init #'skroad--link-init
 :on-create #'skroad--link-create
 :on-destroy #'skroad--link-destroy
 :on-activate #'skroad--browse-skroad-link
 :start-delim "[[" :end-delim "]]"
 :keymap (define-keymap
           "l" #'skroad--live-link-to-dead
           "r" #'skroad--cmd-rename-remote-node
           )
 :use 'skroad--text-delimited-non-title
 :use 'skroad--text-render-delimited-zoned
 :use 'skroad--text-indexed
 )

(defun skroad--dead-link-to-live ()
  "Transform all dead links with payload LINK to live links."
  (interactive)
  (skroad--with-link-at-point
   (funcall (get 'skroad-dead 'replace-with-type) link 'skroad-live)))

(skroad--define-text-type
 'skroad-dead
 :doc "Dead (i.e. revivable placeholder) link to a skroad node."
 :kbd-doc "<l> liven|<t> textify|<del> delete|<spc> prepend space"
 :use 'skroad-node-link
 :on-init #'skroad--link-init
 :on-create #'skroad--link-create
 :on-destroy #'skroad--link-destroy
 :start-delim "[-[" :end-delim "]-]"
 :face 'skroad--dead-link-face
 :keymap (define-keymap
           "l" #'skroad--dead-link-to-live)
 :use 'skroad--text-delimited-non-title
 :use 'skroad--text-render-delimited-zoned
 :use 'skroad--text-indexed
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--comment-url ()
  "Debuttonize the URL at point by inserting a space after the prefix."
  (interactive)
  (skroad--with-zone
    (save-mark-and-excursion
      (goto-char start)
      (search-forward "//" end)
      (insert " "))))

(skroad--define-text-type
 'skroad-url-link
 :doc "URL."
 :kbd-doc "<return> go|<t> textify|<del> delete|<spc> prepend space"
 :use 'skroad-link
 :help-echo "External link."
 :payload-regex
 "\\(\\(?:http\\(?:s?://\\)\\|ftp://\\|file://\\|magnet:\\)[^\n\t\s]+\\)"
 :on-activate #'browse-url
 :keymap (define-keymap
           "t" #'skroad--comment-url)
 :use 'skroad--text-delimited-non-title
 :use 'skroad--text-render-delimited-zoned
 :use 'skroad--text-indexed
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--hider nil "Text hider overlay.")
(defvar-local skroad--renamer nil "Node renamer overlay.")
(defvar-local skroad--renamer-changes nil "Change group for renamer.")

(defun skroad--renamer-activate (renamer-type start end)
  "Activate the renamer in the current zone."
  (message "Rename node: press <return> to rename, or leave field to cancel.")
  (skroad--deactivate-mark)
  (setq-local cursor-type t)
  (setq skroad--renamer-changes (prepare-change-group))
  (activate-change-group skroad--renamer-changes)
  (setq skroad--hider (make-overlay start end (current-buffer)))
  (overlay-put skroad--hider 'invisible t)
  (goto-char end)
  (insert (concat " " (skroad--atomic-at start) " "))
  (setq-local skroad--renamer (make-overlay end (point) (current-buffer)))
  (overlay-put skroad--renamer 'category renamer-type)
  (goto-char end))

(defun skroad--renamer-deactivate ()
  "Deactivate the renamer if it is currently active."
  (when (skroad--overlay-active-p skroad--renamer)
    (delete-overlay skroad--renamer)
    (skroad--deactivate-mark)
    (undo-amalgamate-change-group skroad--renamer-changes)
    (cancel-change-group skroad--renamer-changes)
    (goto-char (overlay-start skroad--hider))
    (delete-overlay skroad--hider)))

(defun skroad--renamer-cmd-accept-changes ()
  "Accept a proposed renaming."
  (interactive)
  (let ((renamed (field-string-no-properties)))
    (skroad--renamer-deactivate)
    (message (format "renamed: '%s'" renamed))))

(skroad--define-text-type
 'skroad-renamer
 :doc "Base mixin for renamer overlays."
 :mixin t
 :rear-advance t
 :id 'type-name
 :field 'id
 :keymap (define-keymap
           "<remap> <end-of-line>"
           #'(lambda () (interactive) (goto-char (1- (field-end))))
           "RET" #'skroad--renamer-cmd-accept-changes)
 :on-leave '(lambda (pos-from auto)
              (message "Rename node: changes discarded.")
              (skroad--renamer-deactivate))
 )

(skroad--define-text-type
 'skroad-node-remote-renamer
 :doc "Renamer for a changing node title while standing on a link."
 :use 'skroad-renamer
 :face 'skroad--renamer-face
 :before-string " " :after-string " ")

(skroad--define-text-type
 'skroad-node-title-renamer
 :doc "Renamer for a changing node title directly."
 :use 'skroad-renamer
 :face 'skroad--title-renamer-face
 :before-string "" :after-string " \n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--get-title ()
  "Get the current node title from the buffer."
  (buffer-substring-no-properties (point-min) (skroad--get-end-of-line 1)))

;; (skroad--define-text-type
;;  'skroad-node-title
;;  :doc "Node title."
;;  :order 500
;;  :face 'skroad--title-face
;;  :keymap (define-keymap
;;            "RET" #'ignore)
;;  :on-init #'skroad--title-init
;;  :on-create #'skroad--title-create
;;  :on-destroy #'skroad--title-destroy
;;  :on-enter
;;  '(lambda (pos-from auto)
;;     (message (format "title enter from=%s auto=%s" pos-from auto))
;;     (when (and mark-active (not auto)) ;; If region is active, don't enter
;;       (goto-char (skroad--body-start)))) ;; ...stop right under it.
;;  :on-leave
;;  '(lambda (pos-from auto)
;;     (message (format "title leave from=%s auto=%s" pos-from auto))
;;     (when (and mark-active (not auto)) ;; If region is active, don't leave
;;       (goto-char pos-from) ;; ...jump to its end.
;;       (goto-char (line-end-position))))
;;  :find-any-forward
;;  '(lambda (limit) (when (bobp) (goto-char (skroad--body-start)) t))
;;  :render
;;  '(lambda ()
;;     (set-text-properties
;;      (point-min) (skroad--body-start)
;;      (list 'category type-name
;;            'id type-name ;; there can only be one
;;            'face face
;;            'data (skroad--get-title))))
;;  :use 'skroad--text-rendered
;;  :for-all-in-region-forward
;;  '(lambda (start end f)
;;     (when (eq start (point-min)) (funcall f (skroad--get-title))))
;;  :use 'skroad--text-indexed
;;  )

(defun skroad--cmd-change-node-title ()
  "Activate the renamer for the current node's title."
  (interactive)
  (skroad--renamer-activate
   'skroad-node-title-renamer (point-min) (skroad--body-start)))

(skroad--define-text-type
 'skroad-node-title
 :doc "Node title."
 :kbd-doc "<r> rename"
 :use 'skroad-atomic
 :order 500
 :keymap
 (define-keymap
   "r" #'skroad--cmd-change-node-title
   "<tab>" #'skroad--cmd-atomics-jump-to-next-link
   "RET" #'ignore
   "SPC" #'ignore
   "<deletechar>" #'ignore "<backspace>" #'ignore
   "<remap> <set-mark-command>" #'ignore
   "<remap> <self-insert-command>" #'ignore
   "<remap> <yank>" #'ignore
   "<remap> <kill-region>" #'ignore
   )
 :face 'skroad--title-face
 :read-only "Title must be changed via rename command!"
 :find-any-forward
 '(lambda (limit) (when (bobp) (goto-char (skroad--body-start)) t))
 :render
 '(lambda ()
    (set-text-properties
     (point-min) (skroad--body-start)
     (list 'category type-name
           'id type-name ;; there can only be one
           'face face
           'data (skroad--get-title))))
 :use 'skroad--text-rendered
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local skroad--pre-command-snapshot (list (point-min) nil nil)
  "Point, zone at point, and type at point prior to a command.")

(defun skroad--point-state ()
  "Return a snapshot of the current point, zone, and type."
  (list (point) (skroad--zone-at) (skroad--type-at)))

(defun skroad--motion (prev &optional auto)
  "To be called whenever the zone under the point may have changed."
  (let ((current (skroad--point-state)))
    (seq-let (old-p old-zone old-type p zone type) (append prev current)
      (when
          (cond ;; text type actions `on-leave` and `on-enter` may both fire
           ((not (eq old-zone zone)) ;; point moved or text changed under it
            (when old-zone ;; point was in a zone, but has left it
              (skroad--type-action old-type 'on-leave old-p auto))
            (when zone ;; point has entered a different zone
              (skroad--type-action type 'on-enter old-p auto))
            t)
           ((and (not (eq old-p p)) old-zone) ;; moved but remained in zone
            (skroad--type-action old-type 'on-move old-p auto)
            t))
        ;; If done moving point, and we went over alt-mark to mark, jump it:
        (when (and mark-active skroad--alt-mark (eq p (point)) (eq p (mark)))
          (if (< skroad--alt-mark p) (forward-char) (backward-char)))
        (skroad--motion current t))) ;; Handle possible auto zone change
    t))

(defun skroad--adjust-mark-if-present ()
  "Put mark and alt-mark in the right order, and show/hide selector."
  (cond
   (mark-active
    (skroad--selector-hide)
    (let ((m (mark)) (am skroad--alt-mark) (p (point)))
      (when (and am (> (abs (- p am)) (abs (- p m))))
        (set-mark am)
        (setq-local skroad--alt-mark m))))
   (t
    (skroad--selector-show)
    (setq-local skroad--alt-mark nil))))

(defun skroad--pre-command-hook ()
  "Triggers prior to every user-interactive command."
  (setq-local mouse-highlight nil)
  (setq-local skroad--pre-command-snapshot (skroad--point-state)))

(defun skroad--post-command-hook ()
  "Triggers following every user-interactive command."
  (skroad--motion skroad--pre-command-snapshot)
  (skroad--adjust-mark-if-present) ;; swap mark and alt-mark if needed
  (skroad--update-local-index) ;; TODO: do it in save hook?
  (unless mark-active (setq-local mouse-highlight t)))

(defun skroad--scroll-hook (window start)
  "Triggers when a buffer scrolls."
  (setq-local header-line-format ;; Float the title if it isn't in view
              (when (and skroad--floating-title-enable (> start 1))
                (buffer-substring (point-min)
                                  (skroad--get-end-of-line 1)))))

(when skroad--debug
  (defadvice skroad--post-command-hook (around intercept activate)
    (condition-case err
        ad-do-it
      ;; Let the debugger run
      ((debug error) (signal (car err) (cdr err))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skroad--find-word-boundary (pos limit)
  "Function for use in `find-word-boundary-function-table'."
  (save-mark-and-excursion
    (let ((atomic (skroad--atomic-at pos))
          (fwd (<= pos limit)))
      (cond ((and atomic fwd) (goto-char (skroad--zone-end pos)))
            (atomic (goto-char (skroad--zone-start pos)))
            (fwd (forward-word-strictly))
            (t (backward-word-strictly)))
      (point))))

(defconst skroad--find-word-boundary-function-table
  (let ((tab (make-char-table nil)))
    (set-char-table-range tab t #'skroad--find-word-boundary)
    tab)
  "Assigned to `find-word-boundary-function-table' in skroad mode.")

;; TODO: does this need with-silent-modifications for textmode temp buffers
;;       where skroad--silence-modifications is not in effect?
(defun skroad--yank-handler (category start end)
  "Handler for use with `yank-handled-properties`."
  (message (format "yank! c=%s" category))
  (skroad--with-whole-lines start end
    (remove-list-of-text-properties
     start-expanded end-expanded skroad--text-properties)
    (font-lock-ensure start-expanded end-expanded))
  (skroad--deactivate-mark))

(defun skroad--open-node ()
  "Open a skroad node."
  (skroad--init-font-lock)
  (skroad--init-local-index)
  (face-remap-set-base 'header-line 'skroad--title-face)
  )

(define-derived-mode skroad-mode text-mode "Skroad"
  ;; Prohibit change hooks firing when only text properties have changed:
  (skroad--silence-modifications 'put-text-property)
  (skroad--silence-modifications 'add-text-properties)
  (skroad--silence-modifications 'remove-text-properties)
  (skroad--silence-modifications 'remove-list-of-text-properties)
  (skroad--silence-modifications 'set-text-properties)
  (skroad--silence-modifications 'add-face-text-property)
  
  ;; Zap properties during unfontification:
  (setq-local font-lock-extra-managed-props skroad--text-properties)
  
  ;; Zap properties and refontify during yank.
  (setq-local yank-handled-properties '((id . skroad--yank-handler)))

  ;; Buffer-local hooks:
  (add-hook 'before-change-functions 'skroad--before-change-function nil t)
  (add-hook 'after-change-functions 'skroad--after-change-function nil t)
  (add-hook 'pre-command-hook 'skroad--pre-command-hook nil t)
  (add-hook 'post-command-hook 'skroad--post-command-hook nil t)
  (add-hook 'window-scroll-functions 'skroad--scroll-hook nil t)
  
  ;; Overlay for when a link is under the point. Initially inactive:
  (setq-local skroad--selector (make-overlay (point-min) (point-min)))
  (skroad--selector-deactivate)

  ;; Properties for selector overlay
  (dolist (p skroad--selector-properties)
    (overlay-put skroad--selector (car p) (cadr p)))

  ;; Keymap:
  (use-local-map skroad--mode-keymap)

  ;; Handle word boundaries correctly (links are treated as unitary words) :
  (setq-local find-word-boundary-function-table
              skroad--find-word-boundary-function-table)

  ;; Buffer-local hooks:
  (add-hook 'skroad-mode-hook 'skroad--open-node 0 t)
  )

(provide 'skroad)
