;;; kubernetes-replicasets.el --- Rendering for Kubernetes replicasets.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-state)

;; Incremental replicasets refresher with paging and progress updates.
(defun kubernetes-replicasets-refresh (&optional interactive)
  (unless (poll-process-live-p kubernetes--global-process-ledger 'replicasets)
    (set-process-for-resource kubernetes--global-process-ledger 'replicasets t)
    (let* ((state (kubernetes-state))
           (ns (kubernetes-state--get state 'current-namespace))
           (accum '())
           (started (current-time))
           (update-state
            (lambda (page)
              (-let* (((&alist 'items items
                               'remainingItemCount rem
                               'continue cont)
                       page)
                      (items (append items nil)))
                (setq accum (append accum items))
                (kubernetes-state-update-replicasets `((items . ,(vconcat accum))
                                                       (remainingItemCount . ,rem)
                                                       (continue . ,cont)
                                                       (started . ,started)))
                (kubernetes--info "Replicasets page: items=%d rem=%s cont=%s accum=%d"
                                  (length items) (or rem "-") (if (and cont (stringp cont) (not (string-empty-p cont))) "Y" "N") (length accum))
                (kubernetes-state-trigger-redraw)))))
      (kubernetes--info "Replicasets refresh started: ns=%s chunk=%s" (or ns "<all>") kubernetes-list-chunk-size)
      (kubernetes-progress-start '(replicasets))
      (kubernetes--info "Replicasets fetching first page…")
      (kubernetes-kubectl-list-paged-replicasets
       state kubernetes-list-chunk-size
       update-state
       (lambda ()
         (kubernetes--info "Replicasets complete: total=%d took=%s" (length accum) (kubernetes--time-diff-string started (current-time)))
         (kubernetes-progress-tick 'replicasets)
         (release-process-for-resource kubernetes--global-process-ledger 'replicasets)
         (when interactive
           (message "Updated replicasets."))))))
  nil)

(defun kubernetes-replicasets-refresh-now (&optional interactive)
  (interactive "p")
  (kubernetes-replicasets-refresh interactive))

(provide 'kubernetes-replicasets)

;;; kubernetes-replicasets.el ends here
