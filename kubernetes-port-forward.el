;;; kubernetes-port-forward.el --- Port-forward helpers for Kubernetes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-kubectl)
(require 'kubernetes-process)
(require 'kubernetes-ast)
(require 'kubernetes-vars)

(defvar kubernetes-port-forward-buffer-prefix "*kubernetes port-forward: "
  "Prefix for Kubernetes port-forward buffers.")

(defun kubernetes-port-forward--resolve-pod (state resource-type resource-name)
  "Resolve a pod name from RESOURCE-TYPE and RESOURCE-NAME using STATE.
If RESOURCE-TYPE is \"pod\", return RESOURCE-NAME.
If RESOURCE-TYPE is \"deployment\", choose a running pod owned by the deployment.
Prompt when multiple candidates are available."
  (cond
   ((string= resource-type "pod") resource-name)
   ((string= resource-type "deployment")
    (let* ((pods (kubernetes-utils--get-all-pods-for-owner "deployment" resource-name state))
           (pod-names (mapcar (lambda (pod)
                                (alist-get 'name (alist-get 'metadata pod)))
                              pods)))
      (cond
       ((null pod-names)
        (user-error "No pods found for deployment %s" resource-name))
       ((= (length pod-names) 1)
        (car pod-names))
       (t
        (completing-read (format "Pod for deployment %s: " resource-name)
                         pod-names nil t)))))
   (t
    (user-error "Port-forward supports pod or deployment; found: %s" resource-type))))

(defun kubernetes-port-forward--buffer-name (ns pod-name local target)
  (let* ((ns-prefix (if ns (format "%s/" ns) ""))
         (spec (format "%s:%s" local target)))
    (format "%s%s%s %s*" kubernetes-port-forward-buffer-prefix ns-prefix pod-name spec)))

;;;###autoload
(defun kubernetes-port-forward-start (resource local-port target-port)
  "Start a kubectl port-forward from RESOURCE to LOCAL-PORT:TARGET-PORT.

RESOURCE is either a pod name, or a cons (TYPE . NAME) when called programmatically.
When called interactively, resolves resource at point (pod/deployment) or prompts.
The process runs in its own buffer until manually killed."
  (interactive
   (let* ((state (kubernetes-state))
          (info (or (kubernetes-utils-get-resource-info-at-point)
                    (let* ((type (completing-read "Resource type: " '("pod" "deployment") nil t))
                           (name (kubernetes-utils-get-resource-name state type)))
                      (cons type name))))
          (local (read-number "Local port: "))
          (target (read-number "Target port: ")))
     (list info local target)))
  (let* ((state (kubernetes-state))
         (type (if (consp resource) (car resource) "pod"))
         (name (if (consp resource) (cdr resource) resource))
         (pod-name (kubernetes-port-forward--resolve-pod state type name))
         (ns (kubernetes-state--get state 'current-namespace))
         (args (append (list "port-forward" (format "pod/%s" pod-name)
                             (format "%s:%s" local-port target-port))
                       (kubernetes-kubectl--flags-from-state state)))
         (bufname (kubernetes-port-forward--buffer-name ns pod-name local-port target-port)))
    (kubernetes-utils-process-buffer-start bufname
                                           #'kubernetes-mode
                                           kubernetes-kubectl-executable
                                           args)
    (message "Started port-forward to %s (local %s -> target %s)" pod-name local-port target-port)))

;;;###autoload
(defun kubernetes-port-forward-kill ()
  "Select a running Kubernetes port-forward buffer and kill its process."
  (interactive)
  (let* ((buffers (seq-filter (lambda (b)
                                (string-prefix-p kubernetes-port-forward-buffer-prefix (buffer-name b)))
                              (buffer-list)))
         (names (mapcar #'buffer-name buffers)))
    (if (null buffers)
        (message "No Kubernetes port-forward buffers found")
      (let* ((choice (completing-read "Kill port-forward: " names nil t))
             (buf (get-buffer choice))
             (proc (and buf (get-buffer-process buf))))
        (when buf
          (if proc
              (kubernetes-process-kill-quietly proc)
            (kill-buffer buf))
          (message "Killed %s" choice))))))

(provide 'kubernetes-port-forward)

;;; kubernetes-port-forward.el ends here
