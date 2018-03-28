;;; omnibox.el --- Selection package  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/omnibox
;; Keywords: completion, selection
;; Package-Requires: ((emacs "26.1") (dash "2.13"))
;; Version: 0.0.1

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;; Commentary:
;;

;;; Code:

(require 'frame-local)
(require 'dash)

(defvar omnibox-frame-parameters
  `((no-accept-focus . t)
    (no-focus-on-map . t)
    (width  . 90)
    (height  . 20)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (left-fringe . 0)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (unsplittable . t)
    (top . 100)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (drag-internal-border . t)
    (left-fringe . 0)
    (right-fringe . 0)
    (background-color . "#282C34")
    (no-special-glyphs . t)))

(defvar-local omnibox-ov nil)
(defvar-local omnibox-selection 0)
(defvar-local omnibox-candidates-length 0)

(defun omnibox--filter-command (item)
  (and (commandp item)
       (not (get item 'byte-obsolete-info))
       item))

(defun omnibox--overlay nil
  (or omnibox-ov
      (setq omnibox-ov (make-overlay 1 1))))

(defun omnibox--make-buffer-name (&optional frame)
  (let ((id (frame-parameter frame 'window-id)))
    (concat "* Omnibox-" id)))

(defun omnibox--buffer (&optional frame)
  (get-buffer-create (omnibox--make-buffer-name frame)))

(defun omnibox--modeline nil
  (let* ((selection (number-to-string (1+ omnibox-selection)))
         (length (number-to-string omnibox-candidates-length))
         (state (concat " " selection "/" length " ")))
    (concat
     (propertize " Omnibox-M-x " 'face '(:background "#35ACCE" :foreground "black")
                 'display '(raise 0.15))
     (propertize " " 'display `(space :align-to (- right-fringe ,(length state)) :height 1.5))
     (propertize state 'face '(:background "#35ACCE" :foreground "black")
                 'display '(raise 0.15)))))

(defun omnibox--get-candidates (&optional regexp)
  "."
  (let* ((completion-regexp-list (and regexp (list regexp)))
         (case-fold-search completion-ignore-case))
    (all-completions "" obarray 'commandp)))

(defun omnibox--render-buffer (candidates)
  (setq omnibox-selection 0)
  (setq omnibox-candidates-length (length candidates))
  (with-current-buffer (omnibox--buffer)
    (erase-buffer)
    (dolist (candidate candidates)
      (let ((string (if (symbolp candidate) (symbol-name candidate) candidate)))
        (insert string "\n")))
    (setq mode-line-format '(:eval (omnibox--modeline))
          truncate-lines t
          omnibox-candidates-length (length candidates)
          header-line-format (propertize " " 'display '(space :align-to right-fringe) 'face '(:height 0.3)))
    (omnibox--update-line 0)
    (current-buffer)))

(defun omnibox--update-list-buffer nil
  (-when-let* ((string (frame-local-getq omnibox-read))
               (regexp (replace-regexp-in-string " " ".*?" string))
               (regexp (concat regexp ".*?")))
    (omnibox--render-buffer (omnibox--get-candidates regexp))))

(defun omnibox--update-read-buffer (&optional string)
  (with-current-buffer (get-buffer-create "*SIDE_TEST*")
    (setq mode-line-format nil
          header-line-format nil)
    (erase-buffer)
    (insert (concat (propertize "M-x: " 'face 'minibuffer-prompt)
                    string
                    (propertize " " 'face 'cursor)))
    (current-buffer)))

(defun omnibox-M-x nil
  (interactive)
  (let* ((candidates (omnibox--get-candidates))
         (buffer (omnibox--render-buffer candidates))
         (frame (omnibox--make-frame buffer)))
    (omnibox-mode 1)
    (with-selected-frame frame
      (display-buffer-in-side-window
       (omnibox--update-read-buffer)
       '((side . top) (window-height . 1)))
      )
    ))

(defun omnibox--resolve-params (params)
  (list
   (plist-get params :prompt)
   (plist-get params :candidates)
   (plist-get params :detail)))

(defun omnibox (&rest plist)
  "Omnibox.

\(fn &key PROMPT CANDIDATES DETAIL)."
  (-let* (((prompt candidates detail) (omnibox--resolve-params plist))
          (candidates (if (functionp candidates) (funcall candidates "") candidates))
          (buffer (omnibox--render-buffer candidates))
          (frame (omnibox--make-frame buffer)))
    (omnibox-mode 1)
    (with-selected-frame frame
      (display-buffer-in-side-window
       (omnibox--update-read-buffer)
       '((side . top) (window-height . 1)))
      )
    ))

;; (omnibox :detail "moi" :candidates '("seb" "ok" "coucou") :prompt "seb")

(defun omnibox--make-frame (buffer)
  (-if-let* ((frame (frame-local-getq omnibox-frame)))
      (make-frame-visible frame)
    (let* ((before-make-frame-hook nil)
           (after-make-frame-functions nil)
           (internal-border (round (* (frame-char-width) 1.2)))
           (x (- (/ (frame-pixel-width) 2)
                 (/ (* 90 (frame-char-width)) 2)
                 internal-border))
           (frame (with-current-buffer buffer
                    (make-frame
                     (append `((left . ,x)
                               (internal-border-width . ,internal-border)
                               (default-minibuffer-frame . ,(selected-frame))
                               (minibuffer . ,(minibuffer-window))
                               (parent-frame . ,(selected-frame)))
                             omnibox-frame-parameters))))
           (window (frame-selected-window frame)))
      (redirect-frame-focus frame (selected-frame))
      (set-window-dedicated-p window t)
      (frame-local-setq omnibox-frame frame)
      frame
      )))

(defun omnibox--update-overlay nil
  "."
  (let ((documentation (or (get-text-property (point) 'omnibox-doc)
                           (-some--> (intern (buffer-substring (line-beginning-position)
                                                               (line-end-position)))
                                     (and (functionp it) it)
                                     (documentation it)
                                     (car (split-string it "\n")))
                           "")))
    (setq documentation (concat " " documentation))
    (move-overlay (omnibox--overlay) (line-beginning-position) (line-end-position))
    (overlay-put (omnibox--overlay)
                 'face '(:background "#607D8B" :foreground "black"))
    (overlay-put (omnibox--overlay)
                 'after-string
                 (concat (propertize " " 'display `(space :align-to (- right-fringe ,(string-width documentation)) :height 1.1)
                                     'face '(:background "#607D8B" :foreground "black"))
                         (propertize documentation 'face '(:background "#607D8B" :foreground "black"))
                         (propertize " " 'display `(space :align-to right-fringe))))))

(defun omnibox--disable-overlays nil
  (overlay-put (omnibox--overlay) 'after-string nil)
  (overlay-put (omnibox--overlay) 'face nil))

(defun omnibox--update-line (selection)
  (setq omnibox-selection selection)
  (goto-char 1)
  (forward-line selection)
  (if (= omnibox-candidates-length 0)
      (omnibox--disable-overlays)
    (omnibox--update-overlay)))

(defun omnibox--select nil
  (interactive))

(defun omnibox--change-line (selection)
  (with-selected-window (get-buffer-window (omnibox--buffer) t)
    (omnibox--update-line selection)))

(defun omnibox--next nil
  (interactive)
  (setq omnibox-selection (min (1+ omnibox-selection) (1- omnibox-candidates-length)))
  (omnibox--change-line omnibox-selection))

(defun omnibox--prev nil
  (interactive)
  (setq omnibox-selection (max (1- omnibox-selection) 0))
  (omnibox--change-line omnibox-selection))

(defun omnibox--hide nil
  (-some-> (frame-local-getq omnibox-frame)
           (make-frame-invisible)))

(defun omnibox--abort nil
  (interactive)
  (omnibox-mode -1)
  (omnibox--hide)
  (frame-local-setq omnibox-read nil))

(defun omnibox--insert nil
  (interactive)
  (let* ((char (char-to-string last-command-event))
         (current (or (frame-local-getq omnibox-read) ""))
         (new-string (concat current char)))
    (frame-local-setq omnibox-read new-string)
    (omnibox--update-read-buffer new-string)
    (omnibox--update-list-buffer)
    ))

(defun omnibox--backward-delete nil
  (interactive)
  (-when-let* ((current (frame-local-getq omnibox-read))
               (len (length current))
               (new-string (substring current 0 (max (1- len) 0))))
    (frame-local-setq omnibox-read new-string)
    (omnibox--update-read-buffer new-string)
    (omnibox--update-list-buffer)
    ))

(defvar omnibox-mode-map nil
  "Keymap for ‘omnibox-mode’.")
(unless omnibox-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "\e\e\e" 'omnibox--abort)
    (define-key map "\C-g" 'omnibox--abort)
    (define-key map (kbd "C-n") 'omnibox--next)
    (define-key map (kbd "<down>") 'omnibox--next)
    (define-key map (kbd "C-p") 'omnibox--prev)
    (define-key map (kbd "<up>") 'omnibox--prev)
    (define-key map (kbd "RET") 'omnibox--select)
    (define-key map (kbd "DEL") 'omnibox--backward-delete)
    (define-key map [remap self-insert-command] 'omnibox--insert)
    (setq omnibox-mode-map map)))

(define-minor-mode omnibox-mode
  "Mode for omnibox."
  :init-value nil)

(global-set-key (kbd "C-o") 'omnibox-M-x)

(provide 'omnibox)
;;; omnibox.el ends here
