;;; kubernetes-daemonsets.el --- Rendering for Kubernetes daemonsets.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(require 'kubernetes-core)
(require 'kubernetes-kubectl)
(require 'kubernetes-modes)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)
(require 'kubernetes-yaml)


;; Components

(defconst kubernetes-daemonsets--column-heading
  ["%-45s %10s %10s %10s %6s" "Name Desired Current Ready Age"])

(kubernetes-ast-define-component daemonset-detail (daemonset)
  (-let [(&alist 'metadata (&alist 'namespace ns 'creationTimestamp time))
         daemonset]
    `( (section (namespace nil)
                (nav-prop (:namespace-name ,ns)
                          (key-value 12 "Namespace" ,(propertize ns 'face 'kubernetes-namespace))))
       (key-value 12 "Created" ,time))))

(kubernetes-ast-define-component daemonset-line (state daemonset)
  (-let* ((current-time (kubernetes-state--get state 'current-time))
          (pending-deletion (kubernetes-state--get state 'daemonsets-pending-deletion))
          (marked-daemonsets (kubernetes-state--get state 'marked-daemonsets))

          ((&alist 'metadata (&alist 'name name 'creationTimestamp created-time)
                   'status (&alist 'desiredNumberScheduled desired
                                   'currentNumberScheduled current
                                   'numberReady ready))
           daemonset)
          (current (or current 0))
          (desired (or desired 0))
          (ready (or ready 0))
          ([fmt] kubernetes-daemonsets--column-heading)
          (list-fmt (split-string fmt))
          (line `(line ,(concat
                         ;; Name
                         (format (pop list-fmt) (s-truncate 43 name))
                         " "
                         ;; Desired
                         (format (pop list-fmt) desired)
                         " "
                         ;; Current
                         (let ((next (pop list-fmt)))
                           (cond
                            ((zerop current)
                             (propertize (format next current) 'face 'warning))
                            (t
                             (propertize (format next current) 'face 'kubernetes-dimmed))))
                         " "
                         ;; Ready
                         (let ((next (pop list-fmt)))
                           (cond
                            ((zerop ready)
                             (propertize (format next ready) 'face 'warning))
                            (t
                             (propertize (format next ready) 'face 'kubernetes-dimmed))))
                         " "
                         ;; Age
                         (let ((start (apply #'encode-time (kubernetes-utils-parse-utc-timestamp created-time))))
                           (propertize (format (pop list-fmt) (kubernetes--time-diff-string start current-time))
                                       'face 'kubernetes-dimmed))))))
    `(nav-prop (:daemonset-name ,name)
               (copy-prop ,name
                          ,(cond
                            ((member name pending-deletion)
                             `(propertize (face kubernetes-pending-deletion) ,line))
                            ((member name marked-daemonsets)
                             `(mark-for-delete ,line))
                            ((zerop desired)
                             `(propertize (face kubernetes-dimmed) ,line))
                            (t
                             line))))))

(kubernetes-ast-define-component daemonset (state daemonset)
  `(section (,(intern (kubernetes-state-resource-name daemonset)) t)
            (heading (daemonset-line ,state ,daemonset))
            (section (details nil)
                     (indent
                      (daemonset-detail ,daemonset)
                      (padding)))))

(kubernetes-ast-define-component daemonsets-list (state &optional hidden)
  (-let (((state-set-p &as &alist 'items daemonsets)
          (kubernetes-state--get state 'daemonsets))
         ([fmt labels] kubernetes-daemonsets--column-heading))
    `(section (daemonsets-container ,hidden)
              (header-with-count "Daemonsets" ,daemonsets)
              (indent
               (columnar-loading-container ,daemonsets
                                           ,(propertize
                                             (apply #'format fmt (split-string labels))
                                             'face
                                             'magit-section-heading)
                                           ,(--map `(daemonset ,state ,it) daemonsets)))
              (padding))))

;; Requests and state management

(kubernetes-state-define-refreshers daemonsets)

;; Displaying daemonsets

(defun kubernetes-daemonsets--read-name (state)
  "Read a daemonset name from the user.

STATE is the current application state.

Update the daemonset state if it not set yet."
  (-let* (((&alist 'items daemonsets)
           (or (kubernetes-state--get state 'daemonsets)
               (progn
                 (message "Getting daemonsets...")
                 (let ((response (kubernetes-kubectl-await-on-async state (-partial #'kubernetes-kubectl-get "daemonsets"))))
                   (kubernetes-state-update-daemonsets response)
                   response))))
          (daemonsets (append daemonsets nil))
          (names (-map #'kubernetes-state-resource-name daemonsets)))
    (completing-read "Daemonset: " names nil t)))

;;;###autoload
(defun kubernetes-display-daemonset (daemonset-name state)
  "Display information for a daemonset in a new window.

STATE is the current application state.

DAEMONSET-NAME is the name of the daemonset to display."
  (interactive (let ((state (kubernetes-state)))
                 (list (kubernetes-daemonsets--read-name state) state)))
  (if-let (daemonset (kubernetes-state-lookup-daemonset daemonset-name state))
      (select-window
       (display-buffer
        (kubernetes-yaml-make-buffer kubernetes-display-daemonset-buffer-name daemonset)))
    (error "Unknown daemonset: %s" daemonset-name)))


(provide 'kubernetes-daemonsets)

;;; kubernetes-daemonsets.el ends here
