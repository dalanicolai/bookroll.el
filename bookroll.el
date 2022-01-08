;;; bookroll.el --- Book roll providing continuous scroll for books etc. -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.
;;
;; Author: Daniel Nicolai <dalanicolai@gmail.com>
;; Keywords: files, multimedia
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'svg)

(defgroup bookroll nil
  "Bookroll sutomizations.")

(defcustom br-scroll-fraction 4
  "Set the scroll step size in 1/fraction of page.")

(defvar-local split-regexp "^[0-9]") ;; for column of numbers as placeholders
(defvar-local split-point-offset 1)

(defvar-local overlays nil)
(defvar-local page-sizes nil)
;; (defvar-local image-sizes nil)
(defvar-local page-positions nil)
;; (defvar-local image-positions nil)
(defvar-local number-of-pages 0)

;; We start with the simplest solution (if this gives performance issues then we
;; can optimize/modify it later), that is always display a page triplet around
;; the currently viewed page, except for the first and last pages where we
;; display only a doublet.
(defvar-local currently-displayed-pages nil)

;;; Testing

;; create colored test images
(dotimes (i 3)
  (set (intern (format "im%s" (1+ i))) (let* ((w 800)
                                              (h 1600)
                                              (svg (svg-create w h)))
                                         (svg-rectangle svg 0 0 w h :fill-color (pcase i
                                                                                  (0 "red")
                                                                                  (1 "green")
                                                                                  (2 "blue")))
                                         (svg-image svg))))

;; create column of 'placeholders'
(defun br-test ()
  (interactive)
  (pop-to-buffer "bookroll-test")
  (erase-buffer)
  (bookroll-mode)
  (let ((inhibit-read-only t))
    (setq cursor-type nil)

    (setq page-sizes (make-list 1000 '(800 . 1600)))
    (setq page-positions (let ((sum 0)
                               positions)
                           (dolist (s page-sizes)
                             (push sum positions)
                             (setq sum (+ sum (cdr s))))
                           (nreverse positions)))
    (setq number-of-pages (length page-sizes))

    (dotimes (i 1000)
      (insert (number-to-string i))
      (insert "\n")))

  (br-create-overlays-list t)
  (br-create-placeholders)

  (goto-char (point-min))
  (br-goto-page 1))

;;; Code

;; TODO replace test sizes
(setq page-sizes (make-list 100 '(800 . 1600)))
(setq page-positions (let ((sum 0)
                            positions)
                        (dolist (s page-sizes)
                          (push sum positions)
                          (setq sum (+ sum (cdr s))))
                        (nreverse positions)))
(setq number-of-pages (length page-sizes))

(defun br-image-size (&optional page)
  (nth (- (or page (br-current-page)) 1) page-sizes))

(defun br-image-position (&optional page)
  (nth (- page 1) page-positions))

(defun br-create-empty-page (size)
  (pcase-let* ((`(,w . ,h) size))
               (svg-image (svg-create w h))))

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
  (interactive)
  (let ((i 0)
        (cur-pos (window-vscroll nil t)))
    (while (<= (nth (1+ i) page-positions) (+ cur-pos (/ (window-pixel-height) 2)))
      (setq i (1+ i)))
    (1+ i)))
    ;; (while (<= (print (nth (1+ i) page-positions)) (print (+ cur-pos (/ (window-pixel-height) 2))))
    ;;   (setq i (1+ i)))
    ;; (print (1+ i))))

(defun br-display-page (page)
  (let ((elt (- page 1)))
    (overlay-put (nth elt overlays) 'display (pcase (% page 3)
                                               (0 im1)
                                               (1 im2)
                                               (2 im3)))))

(defun br-undisplay-page (page)
  (let ((elt (- page 1)))
    (overlay-put (nth elt overlays)
                 'display
                  (br-create-empty-page (car page-sizes)))))

(defun br-update-page-triplet (page)
  (let ((display-pages (pcase page
                         (1 '(1 2))
                         ((pred (= number-of-pages)) (list page (- page 1)))
                         (p (list (- p 1) p (+ p 1))))))
    (dolist (p currently-displayed-pages)
      (unless (member p display-pages)
        (br-undisplay-page p)))
    (dolist (p display-pages)
      (br-display-page p))
    (setq currently-displayed-pages display-pages)))

(defun br-goto-page (page)
  (interactive "n")
  (br-update-page-triplet page)
  (let* ((elt (- page 1)))
    (set-window-vscroll nil (nth elt page-positions) t)))

(defun br-scroll-up ()
  (interactive)
  ;; because pages could have different heights, we calculate the step size on each scroll
  ;; TODO define constant scroll size if doc has single page height
  (let ((scroll-step-size (/ (cdr (br-image-size)) br-scroll-fraction)))
    (set-window-vscroll nil (+ (window-vscroll nil t) scroll-step-size) t)
    ;; when current page changed after scrolling then update displayed pages
    (let ((current-page (print (br-current-page))))
        (br-update-page-triplet current-page))))

(defun br-scroll-down ()
  (interactive)
  ;; because pages could have different heights, we calculate the step size on each scroll
  ;; TODO define constant scroll size if doc has single page height
  (let ((scroll-step-size (/ (cdr (br-image-size)) br-scroll-fraction)))
    (set-window-vscroll nil (- (window-vscroll nil t) scroll-step-size) t)
    ;; when current page changed after scrolling then update displayed pages
    (let ((current-page (print (br-current-page))))
      (br-update-page-triplet current-page))))

;; TODO Change to minor mode
(define-derived-mode bookroll-mode special-mode "BookRoll"
  "This is a continuous scroll engine for rendering books.")

(cond ((boundp 'evil-version)
       (evil-define-key 'motion bookroll-mode-map "j" 'br-scroll-up)
       (evil-define-key 'motion bookroll-mode-map "k" 'br-scroll-down))
      (t (define-key bookroll-mode-map "j" 'br-scroll-up)
         (define-key bookroll-mode-map "k" 'br-scroll-down)))
