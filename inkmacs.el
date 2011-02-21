;; inkmacs.el -- experimental emacs/inkscape bridge
;; (c) fsf 2010, 2011
;; author:joakim verona
;; license:gpl

(require 'dbus)
(require 'dbus-introspection)

(require 'dbus-proxy) ;;TODO work around deftest problem

;;Experimental integration between inkscape and emacs using dbus.

;;Currently needs bleeding edge versions of a number of components.
;; - trunk version of inkscape with dbus enabled (see note below)
;; - trunk version of Eieio(needs a change which hasnt been merged downstream)
;; - trunk version of Jan Moringen dbus-proxy 
;; - emacs 23(i use emacs from trunk, but 23 should be ok)

;;If you accept that all this really is bleeding edge for real, and
;;not something i just say, controling inkscape from emacs is rather
;;fun! If you furthermore use my inkscape branch mentioned below,
;;inkmacs even aproaches usable!

;; the long term goal is to make an emacs that does things quickly
;;  that currently inhibits creative flow with inkscape. In
;;  particular I want to make a framework that supports specialized
;;  workflows, such as producing sketches for blog entries and web comics.
;; so, when inspiration hits you: m-x inkscape-blog-sketch, 
;;rather than fiddling about in menus etc until you loose inspiration.

;;, for this we want to:
;; - make the xwidget emacs branch usable, so inkscape can be embedded in emacs
;; - make inkscape support xembed, so it can be embedded in emacs
;; - make an inkscape mode that shows just the canvas
;; - make an emacs inkscape control mode that implements a proper emacs ui on top of inkscape
;; - somehow implement the emacs buffer model with inkscape
;; - implement a form of OLE:
;;  - display svg images inline muse-mode org org mode for example(this is already mostly possible)
;;  - edit the svg inside inkscape when desired

;; very important is to support text editing in emacs. nodes in an outline-mode
;; document should preferably be bound to nodes in the inkscape document.

;; please note that there is an Inkscape branch where I have some
;; bugfixes for the dbus support:
;; lp:~joakim-verona/inkscape/dbus-fixes
;; In particular the ink-org integration wont work at all without the fixes


;;check alive
;;(dbus-ping :session   "org.inkscape" 100)

;;(dbus-introspect-xml :session   "org.inkscape" "/")

;;(dbus-introspect-get-all-nodes :session   "org.inkscape" "/org/inkscape")

;;(dbus-introspect-get-interface :session   "org.inkscape" "/org/inkscape/application" "org.inkscape.application")
;;(dbus-introspect-get-method-names :session   "org.inkscape" "/org/inkscape/application" "org.inkscape.application")
;;(dbus-introspect-get-method-names  :session "org.inkscape"  "/org/inkscape/desktop_24" "org.inkscape.document")
;; (dbus-introspect-get-method  :session "org.inkscape"  "/org/inkscape/desktop_24" "org.inkscape.document" "rectangle")

;;(dbus-introspect :session "org.inkscape" "/org/inkscape")
(defcustom inkscape-path
  "/home/joakim/build_myprojs/inkscape/inkscape/src/inkscape"
  "path to dbus-enabled inkscape")

(defvar inkscape-desktop-name "desktop_0"
  "this is currently hardcoded, since the inkscape dbus api isnt feature complete yet")

(defvar inkscape-desktop nil)
(defvar inkscape-application nil)
(defvar inkscape-proxies-registered nil)

(defun inkscape-alive ()
  (dbus-ping :session   "org.inkscape" 100))


(defun inkscape-register-proxies ()
  (interactive)
  (message "registering dbus proxies")
  (setq inkscape-application (inkscape-app-dbus-proxy-create))
  (setq inkscape-desktop (inkscape-document-dbus-proxy-create inkscape-desktop-name))
  (message "registering inkscape verb proxies")
  (inkscape-make-verb-list)
  (message "emacs-inkscape bridge ready for action!")
  (setq inkscape-proxies-registered t))

(defun inkscape-start ()
  (interactive)
  (if (not (dbus-ping :session   "org.inkscape" 100))
      (let*
          ((ping-count 0)
           (inkproc (start-process "inkscape" "*inkscape*" inkscape-path )))
        (while (not(inkscape-alive))
          (setq ping-count (+ 1 ping-count))
          (message "pinging inkscape %d" ping-count)
          (inkscape-sleep-for)))
    (message "inkscape already started and responding to ping")
    (unless inkscape-proxies-registered (inkscape-register-proxies))))

(defun inkscape-sleep-for ()
  (sleep-for 10);;this call doesnt seem to wait at all.
  (read-string "sleep-for, just press enter");; so do this as a workaround
  ;;if the sleep-for doesnt work, we get a busy loop and we DOS dbus, and all manner of bad things happen
  ;;TODO therefore ive factored out the sleep-for until a proper resolution is found
  )

;; call-verb support
;; inkscape doesnt export all functionality through proper dbus interfaces atm.
;; there is an older "verb" interface, and a dbus bridge.
;; here is some code that tries to aproximate the dbus-proxy api for the verb api

(defun inkscape-make-verb-list ()
  (start-process "inkscape-verb-list" "*inkscape-verb-list*" inkscape-path "--verb-list")
  (with-current-buffer  "*inkscape-verb-list*"
    (goto-char (point-min))
    (while (re-search-forward  "^\\([^:]+\\):\\(.*\\)$" nil t)
      (message "[%s][%s]" (match-string 1) (match-string 2))
      (inkscape-make-verb-method (match-string 1)(match-string 2)))))

(defun inkscape-make-verb-method (name doc)
  (eval `(defmethod ,(intern (inkscape-transform-method-name "inkverb" name))
           ((this 'org\.freedesktop\.DBus\.Introspectable-org\.freedesktop\.DBus\.Properties-org\.inkscape\.document
             ;;,(object-class inkscape-desktop) ;;inkscape-desktop must be initialized
                  ))
           ,doc
           (inkdoc-call-verb this ,name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;creating the dbus proxies, using Jan Moringen fantastic
;;dbus-proxy library. The way emacs dbus integration was meant to be


(defun inkscape-transform-method-name (prefix name)
  "Transform NAME. prepend PREFIX.
 PREFIX can be inkapp- or inkdoc- for
 example. un-camelcase. switch underscore to dash."
  (concat prefix "-" (replace-regexp-in-string "_" "-" (dbus-proxy-transform-camel-case name))))




(defun inkscape-app-dbus-proxy-create ()
  "create dbus-proxy to talk to inkscape app"
  (let* ((dbus-proxy-transform-method-name-function (lambda (name) (inkscape-transform-method-name "inkapp" name)))
         (obj (dbus-proxy-make-remote-proxy
               :session "org.inkscape"
               "/org/inkscape/application" t)))
    obj))


(defun inkscape-document-dbus-proxy-create (desktop)
  "create dbus-proxy to talk to inkscape desktop"
  (let* ((dbus-proxy-transform-method-name-function (lambda (name) (inkscape-transform-method-name "inkdoc" name)))
         (obj (dbus-proxy-make-remote-proxy
               :session "org.inkscape"
               (concat "/org/inkscape/" desktop) t)))
    obj))

;;TODO
;; should be buffer local
;; seems to create an inkscape instance mysteriously
;;(setq inkscape-desktop (inkscape-document-dbus-proxy-create "desktop_0"))
;;(setq inkscape-desktop-1 (inkscape-document-dbus-proxy-create "desktop_1"))

(defun inkscape-local-instance ()
  "create a buffer local instance of inkscape"
  ;;TODO this needs more cleverness
  ;;handle closing of ink desktop etc
  (unless inkscape-desktop
    (let ((newdesk (car (last (split-string (inkapp-desktop-new inkscape-application ) "/")))))
      (set (make-local-variable 'inkscape-desktop) (inkscape-document-dbus-proxy-create newdesk)))))

(defun inkscape-local-instance-close ()
  (inkdoc-close inkscape-desktop)
  (setq inkscape-desktop nil))
  
;;;;;;;;;;;;;;;;;;;;;;;;;,,
;;image mode adapter code
(defun inkscape-open-buffer-file ()
  (interactive)
  ;;TODO check that the buffer contains a SVG file
  ;;BUG funnily crashes if called twice on the same desktop object(not reproducible)
  ;;inkdoc-load is awkward:
  ;; - 1st open happens inside "virgin" desktop
  ;; - subsequent opens happen in new desktops
  ;; - the resulting desktop name isnt returned
  (inkscape-local-instance)
  (inkdoc-load inkscape-desktop  (buffer-file-name)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;inkscape org integration - the pride of inkmacs
;;

;; strategy:
;;   - tree level 1 represents the file
;;   - tree level 2 is a column heading
;;   - tree level 3 and lower are placed in the column

;; inkscape text is a little bit unintuitive:
;; text objects are realy a set of text span objects with separate prperties
;; but tde api doesnt ouite reflect that because its inconvenient anyway
;; furthermore the spans dont change after you create them

;; for the purpose of inkorg, its nicer if we handle formating and
;; wordwrap inside inkscape. to get that we need a text object and
;; another linked object which determines the shape. they are both handled separately. 

;; also note that svg 1.2 isnt finalized so convert to text before publishing:
;; http://wiki.inkscape.org/wiki/index.php/FAQ#What_about_flowed_text.3F

;; tip: create a ospecal txt layer before inkorg-create-text-group

;; these values are used to place new nodes
;; old nodes will retain their placement
(defvar inkorg-x 0)
(defvar inkorg-y 0)

(defun inkorg-create-or-update-text (do-tree)
  (interactive "P")
  (if do-tree
      (inkorg-create-text-group)
    (inkorg-create-or-update-text-node))
 ) 

(defun inkorg-create-text-group()
  (interactive)
  "traverse an org tree and create text nodes.
the nodes will be placed on the document canvas according to a simple pattern
the first time. the nodes will retain position later."
  (setq inkorg-x 0  inkorg-y 0);;todo refactor

  (org-map-entries 'inkorg-create-or-update-text-node nil 'tree 'comment)
  )

;;(defvar inkorg-select 'keep-subtree);;todo should be let bound local
(defvar inkorg-select-start-level 0);;todo should be let bound local

(defun inkorg-select-skip ()
  "determine node skippage"
  (cond
   ((eq inkorg-select 'keep-siblings)
    (if (= (org-outline-level) inkorg-select-start-level) nil t))
   ((eq inkorg-select 'keep-sibling-subtrees)
    (if (>= (org-outline-level) inkorg-select-start-level) nil t))
   ((eq inkorg-select 'keep-subtree) nil)
   (t nil) )
  )

(defun inkorg-select-tree (inkorg-select)
  "select the nodes in inkscape corresponding to the org tree"
  (interactive
   (list (if current-prefix-arg (read (completing-read "keep:" '("keep-sibling-subtrees" "keep-siblings" "keep-subtree") )))))
  (save-excursion
    (org-back-to-heading)
    (setq inkorg-select-start-level (org-outline-level))
    (unless (or (= 1 (org-outline-level)) (equal inkorg-select 'keep-subtree))
      (org-up-heading-all 100))
    (org-map-entries 'inkorg-select-node nil 'tree 'inkorg-select-skip))
  )

(defun inkorg-select-node ()
  "select the text and flow objects in inkscape corresponding to the org node"
  (let* ((id (org-id-get nil t)))
    (inkdoc-selection-add inkscape-desktop id)
    (inkdoc-selection-add inkscape-desktop (concat id "-flow"))
    ))
    
    

(defun inkorg-entry-text ()
  "extract text from current org node, in a format suitable to
create an inkscap text node from.
asterisks and properties are removed."
  (let ((text (concat (org-get-heading) "\n" (org-get-entry))))
    (set-text-properties 0 (length text) nil text )
    (substring text 0 (string-match org-property-start-re text))))


(defun inkorg-create-text-node ()
  "create a corresponding inkscape text node from the current org node."
  (interactive)

  ;;placement
  (if (= 2 (org-outline-level));;todo refactor
      (progn
        (setq inkorg-x (+ 400 inkorg-x))
        (setq inkorg-y 0)))
  (setq inkorg-y (+ 200 inkorg-y))

  ;;create text node
  (let* ((text (inkorg-entry-text))
         (id (org-id-get nil t))
         (flow-node (inkdoc-rectangle inkscape-desktop inkorg-x inkorg-y 200 200))  ;; create text flow rectangle
         (flow-id (concat id "-flow"))
         (text-node (inkdoc-text inkscape-desktop inkorg-x inkorg-y text)))
    (inkdoc-set-attribute inkscape-desktop text-node "id" id)
    (inkdoc-set-attribute inkscape-desktop flow-node "id" flow-id)
    ;;link text flow frame and text node
    (inkdoc-set-color inkscape-desktop flow-id 255 255 255 t)
    ;;   select both objects
    (inkdoc-selection-set-list inkscape-desktop (list flow-id id))
    (inkverb-object-flow-text inkscape-desktop) ;;text sshall be flowed in the frame
    ;; were not finished because the text id has changed so change it back
    ;; we rely on the new flow object being selected which seems fragile
    (inkdoc-set-attribute inkscape-desktop     (car (inkdoc-selection-get inkscape-desktop)) "id" id)
    (inkdoc-selection-clear  inkscape-desktop)
    ))

(defun inkorg-create-or-update-text-node ()
  "create a corresponding inkscape text node from the current org
node, or update the node if it already exists."
  (interactive);bind to c-m-x
  (let* ((text (inkorg-entry-text))
        (id (org-id-get nil t)))
    (if (inkmacs-node-exists inkscape-desktop id)
        (inkdoc-set-text inkscape-desktop id (inkorg-entry-text))
      (inkorg-create-text-node))))

(define-minor-mode inkorg-mode "inkorg" nil " inkorg"
  '(( "\e\C-x" . inkorg-create-or-update-text))
  (if inkorg-mode (inkscape-local-instance)
    (inkscape-local-instance-close))
    
  )
  
(defun inkmacs-node-exists (desk name)
  "see if an inkscape object exists"
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



(provide 'inkscape)

