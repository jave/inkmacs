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

;;TODO this doesnt work out of the box for uninstalled inkscape
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


(defun inkscape-register-proxies (&optional force)
  "Register proxys. FORCE re-registers methods.
This is useful if Inkscape has aquired new methods, or the
previous proxy creation run failed for some reason."
  (interactive)
  (if (or force (not inkscape-proxies-registered))
      (progn
        (message "registering dbus proxies")
        (setq inkscape-application (inkscape-app-dbus-proxy-create)) ;;seems to bring up an inkscape window
        (setq inkscape-desktop-dummy (inkscape-document-dbus-proxy-create inkscape-desktop-name))
        (message "registering inkscape verb proxies")
        (inkscape-make-verb-list)
        (message "emacs-inkscape bridge ready for action!")
        (setq inkscape-proxies-registered t))))

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

(defun inkscape-document-dbus-proxy-create (desktop &optional session)
  "Create dbus-proxy to talk to inkscape DESKTOP.
slow the first time, then not so bad."
  (let* ((dbus-proxy-transform-method-name-function (lambda (name) (inkscape-transform-method-name "inkdoc" name)))
         (obj (dbus-proxy-make-remote-proxy
               :session (or session "org.inkscape")
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

(defvar inkmacs-dummy-process nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun inkmacs-init ()
  (interactive)
  ;;it seems that an inkscape process must be running to expose the dbus api
  ;;it shouldnt really be necessary so investigate TODO
  (unless inkmacs-dummy-process
    (setq inkmacs-dummy-process (start-process "inkscape" "*inkscape process*" inkscape-path ))
    ;;inkscape must acutally be active
    ;;TODO dbus ping in a loop or something rather than a sleep
    (sleep-for 20))
  ;;initialize bridge
  ;;since we need the dummy desktop in order for the dbus classes to exist
  ;;it must be called before method definitions
  (inkscape-register-proxies)
  (defmethod inkscape-desktop-alive ( (this org\.freedesktop\.DBus\.Introspectable-org\.freedesktop\.DBus\.Properties-org\.inkscape\.document))
    "Check if the desktop is alive."
    ;;todo make this method actually work
    (dbus-introspect-get-method-names  :session "org.inkscape"
                                       (oref this object)
                                       "org.inkscape.document")))

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
    (unless (equal inkorg-select 'keep-subtree)
      (inkorg-root-node));;TODO refactor. support EXPORT_FILE_NAME
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
  "Get the entry text, after heading, to next heading, or eof."
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
  ;;there are some issues with layers. id like to create objs 1st and set layers later
  ;;but thats not reliable
  ;;layer movement is overall being annoying
  (let* ((desk (inkscape-desktop))
         (text (inkorg-entry-text);;TODO enable different text extraction functions
               )
         (dummy     (inkdoc-layer-set desk "inkmacs-flow-layer")    )
         (flow-node (inkdoc-rectangle desk inkorg-x inkorg-y 200 200))  ;; create text flow rectangle TODO enable size formatting
         (flow-id (inkorg-flow-id (org-id-get nil t)))
         (dummy     (inkdoc-layer-set desk "inkmacs-text-layer")    );;TODO this is redundant because flow joins stuff... se later
         (text-node (inkdoc-text desk inkorg-x inkorg-y text))
         (text-id (inkorg-text-id (org-id-get nil t))))
    ;;(inkdoc-move-to-layer desk text-node "inkmacs-text-layer")
    
    (inkdoc-set-attribute desk text-node "id" text-id)
    ;;(inkdoc-move-to-layer desk flow-node "inkmacs-flow-layer")
    (inkdoc-set-attribute desk flow-node "id" flow-id)
    ;;link text flow frame and text node
    (inkdoc-set-color desk flow-id 255 255 255 t) ;;TODO enable formatting of flow frame
    (inkdoc-set-attribute desk flow-id "style" "color:#000000;fill:none;stroke:#7c7c7c;stroke-width:1;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;stroke-dashoffset:0;marker:none;visibility:visible;display:inline;overflow:visible;enable-background:accumulate;fill-opacity:1")
    ;;   select both objects
    (inkdoc-selection-set-list desk (list flow-id text-id))
    (inkverb-object-flow-text desk) ;;text sshall be flowed in the frame
    ;; were not finished because the text id has changed so change it back
    ;; we rely on the new flow object being selected which seems fragile
    (inkdoc-set-attribute desk     (car (inkdoc-selection-get desk)) "id" text-id)
    (inkdoc-selection-clear  desk)
    ;;moving text to text layer needs to be last since the flow operation joins flow and text
    ;; so this splits into 2 layers
    (inkdoc-move-to-layer desk text-id "inkmacs-text-layer");;TODO this inkscape patch didnt get merged upstream yet. check progress
    ))


(defun inkorg-create-or-update-text-node ()
  "create a corresponding inkscape text node from the current org
node, or update the node if it already exists."
  (interactive);bind to c-m-x
  (let* ((text (inkorg-entry-text))
         (id (inkorg-text-id (org-id-get nil t))))
    (if (inkmacs-node-exists (inkscape-desktop) id)
        (inkdoc-set-text (inkscape-desktop) id (inkorg-entry-text))
      (inkorg-create-text-node))
    (inkorg-apply-text-formatting id 0 (string-match "\n" text))
    ;; TODO allow different styles. this one just bollds up to 1st newline
    ))

(defun inkorg-apply-text-formatting (text-id range-start range-end )
  "experimental. needs patched inkscape"
  (inkdoc-text-apply-style (inkscape-desktop) text-id
                           range-start range-end "font-weight" "bold"))


(defun inkorg-export-svg-1.1 ()
  "export to svg 1.1 under a new name.
the new file is un-flowrooted an scoured.
Then it should render well in batik and firefox hopefully."
  (interactive)
  ;;this is not real code yet.
  (unless (inkscape-desktop) (error "not an inkmacs buffer"))
  (let* ((newname(concat  (buffer-file-name) "scoured.svg")))
    (copy-file (buffer-file-name) newname)
    (find-file newname)
    (inkmacs-edit)
    (select all flow nodes)
    (inkorg-flow-to-text)
    (save the file)
    (scour the file)))

(defun inkorg-flow-to-text ()
  "Convert selected nodes from flow to text. This should normaly
  preserve formatting. This is useful for converting to a SVG 1.1
  format."
  (interactive)
  (inkverb-object-flowtext-to-text (inkscape-desktop)))

(defun inkorg-flow-text-height (flowtext-id)
  "figure out the current height of a flowtext node"
  (interactive)
  (inkverb-object-flowtext-to-text (inkscape-desktop)))


(defun inkorg-svg-file-name ()
  "Figure out which svg file to use in this context."
  ;;TODO try (org-export-get-title-from-subtree) instead)
  ;;				 (org-entry-get (region-beginning)  "EXPORT_FILE_NAME" t)
  (save-excursion
    (let
        ((fileroot (mapconcat (lambda (e) e) (butlast (split-string (buffer-file-name ) "/")) "/"))
         (export-file-name (org-entry-get  (point)  "EXPORT_FILE_NAME" t)))
      (inkorg-root-node)
      (let ((file-name (concat  (or export-file-name
                                    (org-get-heading))
                                ".svg")))
        (set-text-properties 0 (length file-name) nil file-name)
        ;;TODO support relative and absolute filenames
        (if (equal "/" (substring file-name 0 1))
            (setq fileroot ""))
        (concat fileroot "/" file-name)))))


(defun inkorg-root-node ()
  "Move point to the image root.
This is either a node with EXPORT_FILE_NAME set or the level 1 parent."
  ;;TODO thus does not reliably find the property
  (interactive)
  (let
      ((export-file-name (org-entry-get  (point)  "EXPORT_FILE_NAME" t))
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
  '(( "\e\C-x" . inkorg-create-or-update-text)))

(defun inkmacs-node-exists (desk name)
  "See if an inkscape object exists.
Argument DESK inkscape desktop.
Argument NAME name of object."
  ;;inkscpe throws an error if it doesnt, so we catch it instead
  (condition-case err
      (inkdoc-get-attribute   desk name "id")
    (error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connectors

(defun inkmacs-connector (start-id stop-id)
  "make a connector from object with start-id to stop-id."
  (let* ((path (inkdoc-line (inkscape-desktop) 0 0 100 100))) ;;the coords arent important
    (inkdoc-set-attribute (inkscape-desktop) path "inkscape:connection-start" (concat "#" start-id))
    (inkdoc-set-attribute (inkscape-desktop) path "inkscape:connection-start-point" "d4")
    (inkdoc-set-attribute (inkscape-desktop) path "inkscape:connection-end" (concat "#" stop-id))
    (inkdoc-set-attribute (inkscape-desktop) path "inkscape:connection-end-point" "d4")
    (inkdoc-set-attribute (inkscape-desktop) path "inkscape:connector-type" "polyline")
    (inkdoc-set-attribute (inkscape-desktop) path "style"
                          "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; presentation
(defun inkmacs-zoom-id (id)
  "zoom the object with the provided id."
  (interactive (list (completing-read "view id:" (inkmacs-get-view-ids))))
  (inkdoc-selection-set (inkscape-desktop) id)
  (inkverb-zoom-selection  (inkscape-desktop)))


;;display-area functions require a patched inkscape atm
(defun inkmacs-zoom-animate (r0x0 r0y0 r0x1 r0y1
                                  r1x0 r1y0 r1x1 r1y1)
  (inkdoc-document-set-display-area (inkscape-desktop) r0x0 r0y0 r0x1 r0y1 1.0)
  (let* ((step 10)
         (delay 0.5) ;;you probably need to tweak step and delay
         (s1 (/ (- r1x0 r0x0 ) step))
         (s2 (/ (- r1y0 r0y0 ) step))
         (s3 (/ (- r1x1 r0x1 ) step))
         (s4 (/ (- r1y1 r0y1 ) step)))
    
    (loop for i from 0 to step do
          (inkdoc-document-set-display-area (inkscape-desktop) (+ r0x0 (* i s1)) (+ r0y0 (* i s2)) (+ r0x1 (* i s3)) (+ r0y1 (* i s4)) 1.0)
          (sit-for delay))))




(defun inkmacs-zoom-animate-to   ( r1x0 r1y0 r1x1 r1y1)
  (apply  'inkmacs-zoom-animate
          (append (inkdoc-document-get-display-area (inkscape-desktop)) (list r1x0 r1y0 r1x1 r1y1))))

(defun inkmacs-zoom-animate-to-view (view)
  (interactive (list (completing-read "view id:" (inkmacs-get-view-ids))))
  (setq inkmacs-previous-view view)
  (let* ((d (inkscape-desktop))
         (x       (string-to-number (inkdoc-get-attribute d view "x")))
         (y       (string-to-number (inkdoc-get-attribute d view "y")))
         (height       (string-to-number (inkdoc-get-attribute d view "height")))
         (width       (string-to-number (inkdoc-get-attribute d view "width"))))
    (inkmacs-zoom-animate-to x y (+ x width) (+ y height))))


(defun inkmacs-get-view-ids ()
  "user defined areas of interest"
  (org-entry-get-multivalued-property nil "views"))

(defvar inkmacs-previous-view nil)

(defun inkmacs-zoom-next-view ()
  "zoom to next view"
  (interactive)
  (unless inkmacs-previous-view (setq inkmacs-previous-view (car  (inkmacs-get-view-ids))))
  (let* ((last-pos (position-if (lambda (x) (equal x inkmacs-previous-view)) (inkmacs-get-view-ids)))
         (last-pos (if last-pos last-pos 0));;fugly. what it lamely tries to achieve is set last pos to 0 as a default
         (next-view (nth (+ 1 last-pos )
                                        (inkmacs-get-view-ids))))
    (message "zoom to %s" next-view)
    ( inkmacs-zoom-animate-to-view next-view)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; template instantiation







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;signals. you need a very experimental inkscape patch for this
(defun inkmacs-signal-demo ()
  (dbus-register-signal
   :session "org.inkscape" "/org/inkscape/desktop_0"
   "org.inkscape.document" "object_moved"
   'inkmacs-signal-handler
   ))

(defun inkmacs-signal-handler (id)
  (let ((x       (inkdoc-get-attribute inkscape-desktop-dummy id "x"))
        (y       (inkdoc-get-attribute inkscape-desktop-dummy id "y")))
    (message "id:%s x:%s y:%s" id x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;xwidgets. you need a very very experimental inkscape patch for this
;;as well as a very experimental emacs.
(defvar inkmacs-xwidget-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(define-key map [xwidget-event] 'inkmacs-xwidget-handler) ;;use the callback scheme instead
    map)
  
  "Keymap for `inkmacs-xwidget-mode'.")



(define-derived-mode inkmacs-xwidget-mode
  special-mode "inkmacs-xwidget" "inkmacs xwidget mode"
  (setq buffer-read-only t))

(defun inkmacs-xwidget ()
  (interactive)
  (require 'xwidget)
  (switch-to-buffer "xw-inkmacs")
  (insert "I\n")
  (let ((xw  (xwidget-insert (point-min)  'socket "socket" 1000  1000)))
    (xwidget-put xw 'callback 'inkmacs-xwidget-callback))
  (inkmacs-xwidget-mode))

(defun inkmacs-xwidget-callback (xwidget xwidget-event-type)
  (interactive)
  (message "stuff happened to inkmacs xwidget %S" xwidget)
  (cond ( (eq xwidget-event-type 'xembed-ready)
          (let*
              ((xembed-id (nth 3 last-input-event)))
            (message "xembed ready  event: %S xw-id:%s" xembed-id xwidget)
            (cond
             (t ;;here we are supposed to identify the correct xwidget originated the event
              (start-process "xembed" "*xwidget-inkscape process*" inkscape-path  (format "--socket-id=%d" xembed-id) )
              (message "now link manually")
              ))))))

(defun inkmacs-xwidget-set-session (session)
  (interactive "ssession:")
  ;;TODO the linking is sketchy still
  (set (make-local-variable 'inkscape-desktop-instance)
       (inkscape-document-dbus-proxy-create "desktop_0" session)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun inkmacs-make-layer (desk name)
  "make layers"
  (let* ((layer (inkdoc-layer-new (inkscape-desktop) )))
    (inkdoc-set-attribute desk layer "id"  name)
    ;;now the obj has changed name so the old name wont work
    (inkdoc-set-attribute desk name "inkscape:label"  name)))

(defun inkmacs-layer-name ( parent-layer &optional sub-layer)
  (format "inkmacs-%s-layer" (concat parent-layer (if sub-layer (concat "-" sub-layer)))))

(defun inkmacs-prepare-page-layer (desk parent-layer)
  "make text and flow layers and a new parent-layer(page layer)"
  (interactive (list (inkscape-desktop) (read-string "page:")))
  (inkmacs-make-layer desk parent-layer)
  (inkmacs-make-layer desk (inkmacs-layer-name parent-layer "flow"))
  (inkmacs-make-layer desk (inkmacs-layer-name parent-layer "text")))

(defun inkmacs-add-background ()
  (interactive)
  ;;;TODO add bacxground layer with a rectangle. just a stub now
  (let* ((d (inkscape-desktop))
         (layer-name (inkdoc-layer-new d)))
    (inkdoc-layer-change-level d "to_bottom")
    (inkdoc-rectangle d 0 0 1000 1000)
    )
  )
(defcustom inkmacs-sketch-directory "~/inkmacs" "where to store sketches made with inkmacs-sketch")
(defun inkmacs-sketch ()
  "make a sketch with inkmacs!"
  (interactive)
  (require 'inkmacs)
  (inkmacs-init)
  (let ((inkfile (concat inkmacs-sketch-directory (time-stamp-string "%Y-%:m-%:d-%:H%:M") ".svg")))
    (inkmacs-create-empty-svg      inkfile)
    (with-current-buffer 
        (find-file inkfile)
      (image-mode)
      (inkscape-open-buffer-file)))
  )
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
