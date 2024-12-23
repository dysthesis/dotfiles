(setq package-enable-at-startup nil)
(setq inhibit-default-init nil)
(setq native-comp-async-report-warnings-errors nil)  ;; only in Emacs 29.0+

;; file-name-handler-alist Ref: https://github.com/progfolio/.emacs.d#file-name-handler-alist
;; Skipping a bunch of regular expression searching in the file-name-handler-alist should improve start time.
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Garbage Collection, ref: https://github.com/progfolio/.emacs.d#garbage-collection
(setq bkup-gc-cons-threshold gc-cons-threshold
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           ;; gc-cons-threshold 100000000
           gc-cons-threshold (+ bkup-gc-cons-threshold 200000)) ; Restore original plus a little bit more
     (message "gc-cons-threshold & file-name-handler-alist restored")
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

;; UI, ref: https://github.com/progfolio/.emacs.d#ui
;; Implicitly resizing the Emacs frame adds to init time. Fonts larger than the 
;; system default can cause frame resizing, which adds to startup time.
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
