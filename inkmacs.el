;; inkmacs.el -- Inkscape Emacs fusion
;; (c) fsf 2010, 2011
;; author:joakim verona
;; license:gpl


;;; Commentary:
;; 
;;Experimental integration between inkscape and Emacs using dbus.
;; See README.org

;;; Code:

(require 'dbus)
(require 'dbus-introspection)
(require 'dbus-proxy)
(require 'org)
(require 'org-exp)

(defcustom inkscape-path
  "inkscape"
  "Path to dbus-enabled inkscape.")

(defvar inkscape-desktop-name "desktop_0"
  "This is currently hardcoded, since the inkscape dbus api isnt feature complete yet.")

(defvar inkscape-desktop-dummy nil
  "There is one desktop per document. a bit awkward because we need a dummy desktop for proxie creation.
then we have buffer local instances.")

(defvar inkscape-application nil
  "There is only one inkscape application.")

(defvar inkscape-proxies-registered nil
  "The proxies needs creating once.  reset it if the interface changes.")

(defun inkscape-alive ()
  "Check if theres a running inkscape."
  (dbus-ping :session   "org.inkscape" 100))


(defun inkscape-register-proxies ()
  "Register proxys."
  (interactive)
  (unless inkscape-proxies-registered
    (message "registering dbus proxies")
    (setq inkscape-application (inkscape-app-dbus-proxy-create)) ;;seems to bring up an inkscape window
    (setq inkscape-desktop-dummy (inkscape-document-dbus-proxy-create inkscape-desktop-name))
    (message "registering inkscape verb proxies")
    (inkscape-make-verb-list)
    (message "emacs-inkscape bridge ready for action!")
    (setq inkscape-proxies-registered t)))

;; call-verb support

(defun inkscape-make-verb-list ()
  "Create wrappers for the Verb API."
  (get-buffer-create "*inkscape-verb-list*")
  (with-current-buffer  "*inkscape-verb-list*"
    (erase-buffer)
    (call-process inkscape-path nil  "*inkscape-verb-list*" nil "--verb-list")

    (goto-char (point-min))
    (while (re-search-forward  "^\\([^:]+\\):\\(.*\\)$" nil t)
      (message "[%s][%s]" (match-string 1) (match-string 2))
      (inkscape-make-verb-method (match-string 1)(match-string 2)))))

(defun inkscape-make-verb-method (name doc)
  "Create a Verb wrapper.  NAME is the verb DOC a docstring."
  (let
      ((method-name (intern (inkscape-transform-method-name "inkverb" name))))
    (eval `(defmethod ,method-name
             ;;BUG the next line fails in a fresh Emacs.
             ;;  but works when  inkscape-register-proxies is called.
             ;;  and then buffer eval works. hmm.
             ((this    org.freedesktop.DBus.Introspectable-org.freedesktop.DBus.Properties-org.inkscape.document
                       ))
             ,doc
             (inkdoc-call-verb this ,name))
          ;;defmethod doesnt support interactive declarations so i add it afterwards
          )
    (put method-name 'interactive-form '(interactive (list (inkscape-desktop))))))

;;dbus proxies

(defun inkscape-transform-method-name (prefix name)
  "Transform NAME. prepend PREFIX.
PREFIX can be inkapp- or inkdoc- for
 example. un-camelcase. switch underscore to dash."
  (concat prefix "-" (replace-regexp-in-string "_" "-" (dbus-proxy-transform-camel-case name))))

(defun inkscape-app-dbus-proxy-create ()
  "Create dbus-proxy to talk to inkscape app."
  (let* ((dbus-proxy-transform-method-name-function (lambda (name) (inkscape-transform-method-name "inkapp" name)))
         (obj (dbus-proxy-make-remote-proxy
               :session "org.inkscape"
               "/org/inkscape/application" t)))
    obj))

(defun inkscape-document-dbus-proxy-create (desktop)
  "Create dbus-proxy to talk to inkscape DESKTOP.
slow the first time, then not so bad."
  (let* ((dbus-proxy-transform-method-name-function (lambda (name) (inkscape-transform-method-name "inkdoc" name)))
         (obj (dbus-proxy-make-remote-proxy
               :session "org.inkscape"
               (concat "/org/inkscape/" desktop) t)))
    obj))

;;inkscape process management
;;inkscape-desktop
(defvar inkscape-desktop-instances nil)

(defun inkscape-local-instance (file-name &optional force)
  "Create a context local instance of inkscape."
  ;;TODO this needs more cleverness
  ;;handle closing of ink desktop etc
  ;;TODO inkorg mode should support more than one desktop
  ;;todo should also do the file name binding
  (interactive)
  (if (and (not force) (inkscape-desktop))
      (error "There already is a linked inkscape. "))
  (let ((newdesk (car (last (split-string (inkapp-desktop-new inkscape-application ) "/")))))
    (set (make-local-variable 'inkscape-desktop-instance) (inkscape-document-dbus-proxy-create newdesk))
    (setq inkscape-desktop-instances (acons file-name inkscape-desktop-instance inkscape-desktop-instances))
    ;;todo inkdoc-load doesnt like if theres no actual file
    (unless (file-exists-p file-name)
        (inkmacs-create-empty-svg file-name))
    (inkdoc-load inkscape-desktop-instance file-name)))

(defun inkmacs-create-empty-svg (file-name)
  "Create empty svg file."
  (with-temp-file file-name
    (insert
     "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<svg width=\"100%\" height=\"100%\">
</svg>
")
    (buffer-string)
    ))

(defun inkscape-local-instance-close ()
  "Close the local inkscape instance."
  (inkdoc-close (inkscape-desktop))
  (setq inkscape-desktop-instance nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;initialize bridge
;;since we need the dummy desktop in order for the dbus classes to exist
;;it must be called before method definitions
(inkscape-register-proxies)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inkscape-desktop-alive ( (this org\.freedesktop\.DBus\.Introspectable-org\.freedesktop\.DBus\.Properties-org\.inkscape\.document))
  "Check if the desktop is alive."
  ;;todo make this method actually work
  (dbus-introspect-get-method-names  :session "org.inkscape"
                                       (oref this object)
                                       "org.inkscape.document"))

(defun inkscape-desktop ()
  "Return the inkscape desktop suitable for the context.
null if there is no desk. error if there is a broken desk."
  ;;TODO the inkscape instance is user visible and can go away unexpectedly if the user closes it
  ;;so we should do some sanity checking here
  ;;TODO inkorg mode should support more than one desktop
  (let ((desk (if inkorg-mode
                  (cdr (assoc (inkorg-svg-file-name) inkscape-desktop-instances))
                (if (boundp 'inkscape-desktop-instance) inkscape-desktop-instance))))
    (if desk
        (if (inkscape-desktop-alive desk)
            desk
          (error "It seems the desktop is gone. Maybe you closed it."))
      nil)))

(defun inkmacs-edit (force)
  "Inkscape edit the buffer or org tree."
  (interactive "P")
  (cond 
   ((equal 'image-mode major-mode) ;;TODO check svg
    (inkscape-open-buffer-file))
   ((equal 'org-mode major-mode)
    (progn
      (inkorg-mode t)
      (inkscape-local-instance (inkorg-svg-file-name) force)))
   (t (error "Don't know how to inkmacs here."))))

;;;;;;;;;;;;;;;;;;;;;;;;;,,
;;image mode adapter code
(defun  inkscape-open-buffer-file ()
  "Open buffer file on a local inkscape instance."
  (interactive)
  ;;TODO check that the buffer contains a SVG file
  (inkscape-local-instance (buffer-file-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;inkscape org integration - the pride of inkmacs

;; these values are used to place new nodes
;; old nodes will retain their placement
(defvar inkorg-x 0)
(defvar inkorg-y 0)

(defun inkorg-create-or-update-text (do-tree)
  "Update the current org node or subtree.
Argument DO-TREE updates the entire subtree."
  (interactive "P")
  (if do-tree
      (inkorg-create-text-group)
    (progn
      (inkorg-create-or-update-text-node)
      (if (called-interactively-p 'any) (inkorg-select-node 'both)))))


(defun inkorg-create-text-group()
  "traverse an org tree and create text nodes.
the nodes will be placed on the document canvas according to a simple pattern
the first time. the nodes will retain position if repositioned manually later."
  (interactive)
  (setq inkorg-x 0  inkorg-y 0);;todo refactor

  (org-map-entries 'inkorg-create-or-update-text-node nil 'tree 'comment))


(defvar inkorg-select 'keep-subtree);;todo should be let bound local
(defvar inkorg-select-start-level 0);;todo should be let bound local

(defun inkorg-select-skip ()
  "Determine node skippage."
  (cond
   ((eq inkorg-select 'keep-siblings)
    (if (= (org-outline-level) inkorg-select-start-level) nil t))
   ((eq inkorg-select 'keep-sibling-subtrees)
    (if (>= (org-outline-level) inkorg-select-start-level) nil t))
   ((eq inkorg-select 'keep-subtree) nil)
   (t nil) ))


(defun inkorg-select-tree (inkorg-select text-or-flow)
  "Select the nodes in inkscape corresponding to the org tree.
Argument INKORG-SELECT filters the nodes to select."
  (interactive
   (list (if current-prefix-arg
             (read (completing-read "keep:" '("keep-all" "keep-sibling-subtrees" "keep-siblings" "keep-subtree") )))
         (if current-prefix-arg (read (completing-read "text-or-flow:" '("both" "text" "flow" ) )))))
  (save-excursion
    (org-back-to-heading)
    (setq inkorg-select-start-level (org-outline-level))
    (unless (or (= 1 (org-outline-level)) (equal inkorg-select 'keep-subtree))
      (org-up-heading-all 100));;TODO refactor. support EXPORT_FILE_NAME
    (org-map-entries (lambda () (inkorg-select-node text-or-flow))
                     nil 'tree 'inkorg-select-skip)))


(defun inkorg-select-node (selector)
  "Select the text and flow objects in inkscape corresponding to the org node."
  (interactive (list 'both))
  (let* ((id (org-id-get nil t)))
    (if (member selector '(text both) )
        (inkdoc-selection-add  (inkscape-desktop) (inkorg-text-id id)))
    (if (member selector '(flow both) )
        (inkdoc-selection-add (inkscape-desktop)  (inkorg-flow-id id) ))))  



(defun org-get-entry-2 ()
  "Get the entry text, after heading, to nex heading, or eof."
  (save-excursion
    (org-back-to-heading t)
    (let ((p1 (point-at-bol 2))
          (p2 (progn (forward-line) (search-forward "*" nil t))))
      (setq p2 (if (null p2) (point-max)
                 (1- p2)))
      (buffer-substring p1  p2))))

(defun inkorg-entry-text ()
  "Extract text from current org node.
Return a format suitable to
create an inkscape text node from.
asterisks and properties are removed."
  ;;TODO there ought to be some code in org-exp for this somewhere(org-ascii for example)
  (let ((text  (concat (org-get-heading) "\n" (org-get-entry-2))))
    (set-text-properties 0 (length text) nil text )

    (replace-regexp-in-string "\\([^\n]\\)\n\\([^\n]\\)" "\\1 \\2" 
                              (concat
                               (substring text 0 (string-match org-property-start-re text))
                               (if (string-match org-property-end-re text)
                                   (substring text (progn (string-match org-property-end-re text) (match-end 0)) (length text)))))))


(defun inkorg-flow-id (id)
  (concat "inkmacs-flow-" id))

(defun inkorg-text-id (id)
  (concat "inkmacs-text-" id))

(defun inkorg-create-text-node ()
  "Create a corresponding inkscape text node from the current org node."
  (interactive)

  ;;placement ;;TODO refactor, enable different placement algorithms
  (if (= 2 (org-outline-level))
      (progn
        (setq inkorg-x (+ 400 inkorg-x))
        (setq inkorg-y 0)))
  (setq inkorg-y (+ 200 inkorg-y))

  ;;create text node
  (let* ((text (inkorg-entry-text);;TODO enable different text extraction functions
               )
         (id (inkorg-text-id (org-id-get nil t)))
         (flow-node (inkdoc-rectangle (inkscape-desktop) inkorg-x inkorg-y 200 200))  ;; create text flow rectangle TODO enable size formatting
         (flow-id (inkorg-flow-id (org-id-get nil t)))
         (text-node (inkdoc-text (inkscape-desktop) inkorg-x inkorg-y text)))
    (inkdoc-set-attribute (inkscape-desktop) text-node "id" id)
    (inkdoc-set-attribute (inkscape-desktop) flow-node "id" flow-id)
    ;;link text flow frame and text node
    (inkdoc-set-color (inkscape-desktop) flow-id 255 255 255 t) ;;TODO enable formatting of flow frame
    ;;   select both objects
    (inkdoc-selection-set-list (inkscape-desktop) (list flow-id id))
    (inkverb-object-flow-text (inkscape-desktop)) ;;text sshall be flowed in the frame
    ;; were not finished because the text id has changed so change it back
    ;; we rely on the new flow object being selected which seems fragile
    (inkdoc-set-attribute (inkscape-desktop)     (car (inkdoc-selection-get (inkscape-desktop))) "id" id)
    (inkdoc-selection-clear  (inkscape-desktop))
    ))

(defun inkorg-create-or-update-text-node ()
  "create a corresponding inkscape text node from the current org
node, or update the node if it already exists."
  (interactive);bind to c-m-x
  (let* ((text (inkorg-entry-text))
         (id (inkorg-text-id (org-id-get nil t))))
    (if (inkmacs-node-exists (inkscape-desktop) id)
        (inkdoc-set-text (inkscape-desktop) id (inkorg-entry-text))
      (inkorg-create-text-node))))

(defun inkorg-apply-text-formatting ()
  "experimental. needs patched inkscape"
  (interactive)
  (let* ((text (inkorg-entry-text))
         (id (inkorg-text-id (org-id-get nil t))))
    (inkdoc-text-apply-style (inkscape-desktop) id
                             0 10 "font-weight" "bold")    )
  )


(defun inkorg-flow-to-text ()
  "Convert selected nodes from flow to text. This should normaly
  preserve formatting. This is useful for converting to a SVG 1.1
  format."
  (interactive)
  (inkverb-object-flowtext-to-text (inkscape-desktop))
  )

(defun inkorg-svg-file-name ()
  "Figure out which svg file to use in this context."
  ;;TODO try (org-export-get-title-from-subtree) instead)
  ;;				 (org-entry-get (region-beginning)  "EXPORT_FILE_NAME" t)
  (save-excursion
    (let
        ((export-file-name (org-entry-get  (region-beginning)  "EXPORT_FILE_NAME" t)))
      ;;org-entry-property-inherited-from TODO
      (move-beginning-of-line nil)
      (unless  (= 1 (org-outline-level)) 
        (org-up-heading-all 100));;TODO use inkorg-root-node
      (let ((file-name (concat  (or export-file-name
                                     (org-get-heading))
                                ".svg")))
        (set-text-properties 0 (length file-name) nil file-name)
        (concat (mapconcat (lambda (e) e) (butlast (split-string (buffer-file-name ) "/")) "/") "/" file-name)))))


(defun inkorg-root-node ()
  "Move point to the image root.
This is either a node with EXPORT_FILE_NAME set or the level 1 parent."
  (interactive)
  (let
      ((export-file-name (org-entry-get  (region-beginning)  "EXPORT_FILE_NAME" t))
       (ex-point org-entry-property-inherited-from))
    (if export-file-name
        (goto-char  ex-point)
      (progn
        (org-back-to-heading)
        (move-beginning-of-line nil)
        (unless  (= 1 (org-outline-level)) 
          (org-up-heading-all 100))))))

(defun inkorg-delete-orphans ()
  ;;TODO
  ;; - figure out all inkscape nodes somewhen created by inkmacs, set A
  ;; - figure out all org nodes from the inkorg root node, set B
  ;; - remove all As not in B
  ;; this is potentially destructive so prompt for each inksscape node to be removed
  )

  
(define-minor-mode inkorg-mode "inkorg" nil " inkorg"
  '(( "\e\C-x" . inkorg-create-or-update-text))    
  )

(defun inkmacs-node-exists (desk name)
  "See if an inkscape object exists.
Argument DESK inkscape desktop.
Argument NAME name of object."
  ;;inkscpe throws an error if it doesnt, so we catch it instead
  (condition-case err
      (inkdoc-get-attribute   desk name "id")
    (error nil)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;test code
(defun inkscape-dbus-proxy-test ()
  (inkscape-app-dbus-proxy-create)
  (let*
      ((doc (inkscape-document-dbus-proxy-create "desktop_0")))
    (inkdoc-rectangle doc 100 100 100 100)))

;;get all defined inkscape methods
;;(remove-if-not (lambda (x)(string-match "^ink.*-" (symbol-name x)) ) (eieio-all-generic-functions))



(defun  inkscape-test ()
  "Opens inkscape, draws a black rectangle. a dbus compatible
Inkscape needs to be running 1st. this test doesnt use the dbus-proxy."
  (let*
      ((desktop "/org/inkscape/desktop_0")
       (rect (dbus-call-method
              :session "org.inkscape" desktop
              "org.inkscape.document" "rectangle" :int32 100 :int32  100 :int32  100 :int32  100)))))



(provide 'inkmacs)

;;; inkmacs.el ends here
