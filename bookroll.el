;;; bookroll.el --- Book roll providing continuous scroll for books etc. -*- lexical-binding:t -*-
(require 'cl-lib)

(defvar-local split-regexp "^[0-9]") ;; for column of numbers as placeholders
(defvar-local split-point-offset 1)

(defvar-local overlays nil)
(defvar-local page-sizes nil)
(defvar-local page-positions nil)

;; create column of 'placeholders'
;; (dotimes (i 100)
;;   (insert (number-to-string i))
;;   (insert "\n"))

;; create colored test images
;; (dotimes (i 3)
;;   (set (intern (format "im%s" (1+ i))) (let* ((w 800)
;;                                               (h 1600)
;;                                               (svg (svg-create w h)))
;;                                          (svg-rectangle svg 0 0 w h :fill-color (pcase i
;;                                                                                   (0 "red")
;;                                                                                   (1 "green")
;;                                                                                   (2 "blue")))
;;                                          (svg-image svg))))


;; TODO replace test sizes
(setq page-sizes (make-list 100 '(800 . 1600)))
(setq page-positions (let ((sum 0)
                            positions)
                        (dolist (s page-sizes)
                          (push sum positions)
                          (setq sum (+ sum (cdr s))))
                        (nreverse positions)))

(defun br-create-empty-page (size)
  (pcase-let* ((`(,w . ,h) size))
               (svg-page (svg-create w h))))

(defun br-create-overlays-list (&optional include-first)
  "Create list over overlays spread out over the buffer contents.
Pass non-nil value for include-first when the buffer text starts with a match."
  ;; first overlay starts at 1
  (let ((beg (goto-char (point-min))))
    (when include-first
      (setq overlays (list (make-overlay beg
                                         (search-forward-regexp split-regexp
                                                                nil
                                                                t))))
      (setq beg (- (search-forward-regexp split-regexp nil t)
                   1)))
    ;; We want to create an overlay for each page in the `page-sizes' list.
    ;; When include-first is non-nil then the list already contains a first
    ;; overlay. For both lists a last overlay is added after the loop to be sure
    ;; it extends to (point-max).
    (dolist (x (if include-first
                   (cddr page-sizes)
                 (cdr page-sizes)))
      (search-forward-regexp split-regexp nil t)
      (push (make-overlay beg (- (point) 1 split-point-offset)) overlays)
      (setq beg (- (point) 1)))
    (push (make-overlay beg (point-max)) overlays)
    (setq overlays (nreverse overlays))))

;; TODO replace test sizes
(defun br-create-placeholders ()
  (let ((ph (br-create-empty-page '(800 . 1600)))
        (constant-size (cl-every #'eql page-sizes (cdr page-sizes))))
      (dotimes (i (length page-sizes))
        (overlay-put (nth i overlays) 'display (if constant-size
                                                   ph
                                                 (br-create-empty-page (nth i page-sizes)))))))

(defun br-current-page ()
  (let ((i 0)
        (cur-pos (window-vscroll nil t)))
    (while (<= (nth (1+ i) page-positions) cur-pos)
      (setq i (1+ i)))
    (1+ i)))

(defun br-goto-page (n)
  (let ((elt (- n 1)))
    (overlay-put (nth elt overlays) 'display (pcase (% n 3)
                                               (0 im1)
                                               (1 im2)
                                               (2 im3)))
    (set-window-vscroll nil (nth elt page-positions) t)))
