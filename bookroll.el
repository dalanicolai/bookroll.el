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

(defvar-local bookroll-mode-winprops-alist t
  "Alist of windows to window properties.
Each element has the form (WINDOW . ALIST).
See `bookroll-mode-winprops'.")

(defcustom br-scroll-fraction 10
  "Set the scroll step size in 1/fraction of page.")

(defvar-local split-regexp "^[0-9]") ;; for column of numbers as placeholders
(defvar-local split-point-offset 1)

(defvar-local overlays-list nil)
(defvar-local image-sizes nil)
;; (defvar-local image-sizes nil)
(defvar-local image-positions nil)
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

    (setq image-sizes (make-list 1000 '(800 . 1600)))
    (setq image-positions (let ((sum 0)
                               positions)
                           (dolist (s image-sizes)
                             (push sum positions)
                             (setq sum (+ sum (cdr s))))
                           (nreverse positions)))
    (setq number-of-pages (length image-sizes))

    (dotimes (i 1000)
      (insert (number-to-string i))
      (insert "\n")))

  (br-create-overlays-list t)
  (br-create-placeholders)

  (goto-char (point-min))
  (br-goto-page 1))

;;; Code

(defun bookroll-mode-winprops (&optional window cleanup)
  "Return winprops of WINDOW.
A winprops object has the shape (WINDOW . ALIST).
WINDOW defaults to `selected-window' if it displays the current buffer, and
otherwise it defaults to t, used for times when the buffer is not displayed."
  (cond ((null window)
         (setq window
               (if (eq (current-buffer) (window-buffer)) (selected-window) t)))
        ((eq window t))
	      ((not (windowp window))
	       (error "Not a window: %s" window)))
  (when cleanup
    (setq bookroll-mode-winprops-alist
  	      (delq nil (mapcar (lambda (winprop)
			                        (let ((w (car-safe winprop)))
				                        (if (or (not (windowp w)) (window-live-p w))
				                            winprop)))
  			                    bookroll-mode-winprops-alist))))
  (let ((winprops (assq window bookroll-mode-winprops-alist)))
    ;; For new windows, set defaults from the latest.
    (if winprops
        ;; Move window to front.
        (setq bookroll-mode-winprops-alist
              (cons winprops (delq winprops bookroll-mode-winprops-alist)))
      (setq winprops (cons window
                           (copy-alist (cdar bookroll-mode-winprops-alist))))
      ;; Add winprops before running the hook, to avoid inf-loops if the hook
      ;; triggers window-configuration-change-hook.
      (setq bookroll-mode-winprops-alist
            (cons winprops bookroll-mode-winprops-alist))
      (run-hook-with-args 'bookroll-mode-new-window-functions winprops))
    winprops))

(defun bookroll-mode-window-get (prop &optional winprops)
  (declare (gv-setter (lambda (val)
                        `(bookroll-mode-window-put ,prop ,val ,winprops))))
  (unless (consp winprops) (setq winprops (bookroll-mode-winprops winprops)))
  (cdr (assq prop (cdr winprops))))

(defun bookroll-mode-window-put (prop val &optional winprops)
  (unless (consp winprops) (setq winprops (bookroll-mode-winprops winprops)))
  (unless (eq t (car winprops))
    (bookroll-mode-window-put prop val t))
  (setcdr winprops (cons (cons prop val)
                         (delq (assq prop (cdr winprops)) (cdr winprops)))))

(defun bookroll-set-window-vscroll (vscroll)
  (setf (bookroll-mode-window-get 'vscroll) vscroll)
  (set-window-vscroll (selected-window) vscroll t))

(defun bookroll-set-window-hscroll (ncol)
  (setf (bookroll-mode-window-get 'hscroll) ncol)
  (set-window-hscroll (selected-window) ncol))

(defun bookroll-mode-reapply-winprops ()
  ;; When set-window-buffer, set hscroll and vscroll to what they were
  ;; last time the image was displayed in this window.
  (when (listp bookroll-mode-winprops-alist)
    ;; Beware: this call to bookroll-mode-winprops can't be optimized away,
    ;; because it not only gets the winprops data but sets it up if needed
    ;; (e.g. it's used by doc-view to display the image in a new window).
    (let* ((winprops (bookroll-mode-winprops nil t))
           (hscroll (bookroll-mode-window-get 'hscroll winprops))
           (vscroll (bookroll-mode-window-get 'vscroll winprops)))
      (when (image-get-display-property) ;Only do it if we display an image!
	      (if hscroll (set-window-hscroll (selected-window) hscroll))
	      (if vscroll (set-window-vscroll (selected-window) vscroll t))))))

(defun bookroll-mode-setup-winprops ()
  ;; Record current scroll settings.
  (unless (listp bookroll-mode-winprops-alist)
    (setq bookroll-mode-winprops-alist nil))
  (add-hook 'window-configuration-change-hook
	          #'bookroll-mode-reapply-winprops nil t))

;; TODO replace test sizes
(setq image-sizes (make-list 100 '(800 . 1600)))
(setq image-positions (let ((sum 0)
                            positions)
                        (dolist (s image-sizes)
                          (push sum positions)
                          (setq sum (+ sum (cdr s))))
                        (nreverse positions)))
(setq number-of-pages (length image-sizes))

(defun br-image-size (&optional page)
  (nth (- (or page (br-current-page)) 1) image-sizes))

(defun br-image-position (&optional page)
  (nth (- page 1) image-positions))

(defun br-create-empty-page (size)
  (pcase-let* ((`(,w . ,h) size))
               (svg-image (svg-create w h))))

(defun br-create-overlays-list (&optional include-first)
  "Create list of overlays spread out over the buffer contents.
Pass non-nil value for include-first when the buffer text starts with a match."
  ;; first overlay starts at 1
  (let ((beg (goto-char (point-min))))
    (when include-first
      (setq overlays-list (list (make-overlay beg
                                         (search-forward-regexp split-regexp
                                                                nil
                                                                t))))
      (setq beg (- (search-forward-regexp split-regexp nil t)
                   1)))
    ;; We want to create an overlay for each page in the `image-sizes' list.
    ;; When include-first is non-nil then the list already contains a first
    ;; overlay. For both lists a last overlay is added after the loop to be sure
    ;; it extends to (point-max).
    (dolist (x (if include-first
                   (cddr image-sizes)
                 (cdr image-sizes)))
      (search-forward-regexp split-regexp nil t)
      (push (make-overlay beg (- (point) 1 split-point-offset)) overlays-list)
      (setq beg (- (point) 1)))
    (push (make-overlay beg (point-max)) overlays-list)
    ;; (print overlays-list)))
    (setq overlays-list (nreverse overlays-list))))

;; TODO replace test sizes
(defun br-create-placeholders ()
  (let* ((constant-size (cl-every #'eql image-sizes (cdr image-sizes)))
         (ph (when constant-size (br-create-empty-page (car image-sizes)))))
    (dotimes (i (length image-sizes))
      ;; (let ((p (1+ i)));; shift by 1 to match with page numbers
        (overlay-put (nth i overlays-list) 'display (or ph (br-create-empty-page (nth i image-sizes)))))))

(defun br-current-page ()
  (interactive)
  (let ((i 0)
              (cur-pos (window-vscroll nil t)))
          (while (<= (nth (1+ i) image-positions) (+ cur-pos (/ (window-pixel-height) 2)))
            (setq i (1+ i)))
          (1+ i)))
    ;; (while (<= (print (nth (1+ i) image-positions)) (print (+ cur-pos (/ (window-pixel-height) 2))))
    ;;   (setq i (1+ i)))
    ;; (print (1+ i))))

(defun br-display-page (page image)
  (let ((elt (- page 1)))
    (overlay-put (nth elt overlays-list) 'display image)))
                 ;; (pcase (% page 3)
                 ;;                               (0 im1)
                 ;;                               (1 im2)
                 ;;                               (2 im3)))))

(defun br-undisplay-page (page)
  (let ((elt (- page 1)))
    (overlay-put (nth elt overlays-list)
                 'display
                  (br-create-empty-page (car image-sizes)))))

(defun br-update-page-triplet (page)
  (let ((display-pages (pcase page
                         (1 '(1 2))
                         ((pred (= number-of-pages)) (list page (- page 1)))
                         (p (list (- p 1) p (+ p 1))))))
    (dolist (p currently-displayed-pages)
      (unless (member p display-pages)
        (br-undisplay-page p)))
    (dolist (p display-pages)
      ;; TODO separate pdf function from bookroll package
      ;; (br-display-page p (pdf-view-create-page p)))
      (pdf-view-display-triplet (pdf-view-create-page p)))
    (setq currently-displayed-pages display-pages)))

(defun br-goto-page (page)
  (interactive "n")
  ;; (br-update-page-triplet page)
  (pdf-view-display-triplet page)
  (let* ((elt (- page 1)))
    (set-window-vscroll nil (nth elt image-positions) t)))

(defun br-scroll-up ()
;; (defun pdf-view-next-line-or-next-page ()
  (interactive)
  ;; because pages could have different heights, we calculate the step size on each scroll
  ;; TODO define constant scroll size if doc has single page height
  (let ((scroll-step-size (/ (cdr (br-image-size)) br-scroll-fraction)))
    (set-window-vscroll nil (+ (window-vscroll nil t) scroll-step-size) t)
    ;; when current page changed after scrolling then update displayed pages
    (let ((current-page (print (br-current-page))))
      (pdf-view-display-triplet current-page))))

;; TODO separate pdf functions from bookroll package
(defun br-scroll-down ()
;; (defun pdf-view-previous-line-or-previous-page ()
  (interactive)
  ;; because pages could have different heights, we calculate the step size on each scroll
  ;; TODO define constant scroll size if doc has single page height
  (let ((scroll-step-size (/ (cdr (br-image-size)) br-scroll-fraction)))
    (set-window-vscroll nil (- (window-vscroll nil t) scroll-step-size) t)
    ;; when current page changed after scrolling then update displayed pages
    (let ((current-page (print (br-current-page))))
      (pdf-view-display-triplet current-page))))

(defun br-next-page (&optional n)
  "View the next page in the PDF.

Optional parameter N moves N pages forward."
  (interactive "p")
  (br-goto-page (+ (br-current-page)
                   (or n 1))))

(defun br-previous-page (&optional n)
  "View the previous page in the PDF.

Optional parameter N moves N pages backward."
  (interactive "p")
  (br-next-page (- (or n 1))))

(define-minor-mode bookroll-mode
  "This is a continuous scroll engine for rendering books."
  :keymap
  '(("j" . br-scroll-up)
    ("k" . br-scroll-down)))

(when (boundp 'evil-version)
  (evil-define-key 'evilified bookroll-mode-map "j" 'br-scroll-up)
  (evil-define-key 'evilified bookroll-mode-map "k" 'br-scroll-down))
