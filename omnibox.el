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
(defvar-local omnibox--extern nil)

(defmacro omnibox--get (variable)
  (let ((var (intern (format "omnibox-%s" variable))))
    `(frame-local-get ',var (frame-parent))))

(defmacro omnibox--set (variable value)
  (let ((var (intern (format "omnibox-%s" variable))))
    `(frame-local-set ',var ,value (frame-parent))))

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
     (propertize (or (omnibox--get title) " Omnibox ") 'face '(:background "#35ACCE" :foreground "black")
                 'display '(raise 0.15))
     (propertize " " 'display `(space :align-to (- right-fringe ,(length state)) :height 1.5))
     (when (> omnibox-candidates-length 0)
       (propertize state 'face '(:background "#35ACCE" :foreground "black")
                   'display '(raise 0.15))))))

(defun omnibox--render-buffer (candidates)
  (setq omnibox-selection 0)
  (with-current-buffer (omnibox--buffer)
    (erase-buffer)
    (insert (mapconcat 'identity candidates "\n"))
    (setq mode-line-format '(:eval (omnibox--modeline))
          truncate-lines t
          omnibox-candidates-length (omnibox--get candidates-length)
          header-line-format (propertize " " 'display '(space :align-to right-fringe) 'face '(:height 0.3)))
    (omnibox--update-line 0)
    (current-buffer)))

(defun omnibox--update-list-buffer nil
  (-> (omnibox--get input)
      (omnibox--fetch-candidates)
      (omnibox--render-buffer)))

(defun omnibox--update-input-buffer (&optional string)
  (with-current-buffer (get-buffer-create "*SIDE_TEST*")
    (setq mode-line-format nil
          header-line-format nil)
    (erase-buffer)
    (insert (propertize (omnibox--get prompt) 'face 'minibuffer-prompt)
            (or string "")
            (propertize " " 'face 'cursor))
    (current-buffer)))

(defun omnibox--sort (candidates input)
  (-let* ((groups (--group-by (string-prefix-p input it) candidates)))
    (-concat
     (--sort (< (length it) (length other)) (alist-get t groups))
     (--sort (< (length it) (length other)) (alist-get nil groups)))))

(defun omnibox--highlight-common (candidate input)
  (setq candidate (copy-sequence candidate))
  (dolist (word (split-string input " " t))
    (-let* ((match-data (string-match word candidate))
            ((start end) (match-data t)))
      (when (> end start)
        (add-face-text-property start end '(:foreground "#35ACCE") nil candidate))))
  candidate)

(defun omnibox--get-candidates (candidates input)
  (cond
   ((and (functionp candidates) (omnibox--get extern))
    (funcall candidates input (omnibox--get predicate) t))
   ((functionp candidates)
    (funcall candidates input))
   (t candidates)))

(defun omnibox--fetch-candidates (input)
  (let* ((candidates (omnibox--get candidates))
         (candidates (omnibox--get-candidates candidates input))
         (candidates (-take 500 candidates))
         (len (length candidates)))
    (omnibox--set candidates-length len)
    (if (> (length input) 0)
        (-> (--map (omnibox--highlight-common it input) candidates)
            (omnibox--sort input))
      candidates)))

(defun omnibox--resolve-params (params)
  (list
   (plist-get params :prompt)
   (plist-get params :candidates)
   (plist-get params :detail)))

(defun omnibox (&rest plist)
  "Omnibox."
  (-let* (((prompt candidates detail) (omnibox--resolve-params plist)))
    (omnibox--set extern omnibox--extern)
    (omnibox--set prompt prompt)
    (omnibox--set candidates candidates)
    (omnibox--set detail detail)
    (-> (omnibox--fetch-candidates "")
        (omnibox--render-buffer)
        (omnibox--make-frame))
    (omnibox-mode 1)))

(defun omnibox-M-x--doc (candidate)
  (-some--> (intern candidate)
            (and (functionp it) it)
            (documentation it)
            (car (split-string it "\n"))))

;;"\\(myword.*oklm.*\\)\\|\\(oklm.*myword\\)" ?
(defun omnibox--obarray-candidates (input predicate)
  (let* ((regexp (->> (string-trim input)
                      (replace-regexp-in-string " \\|$" ".*?")))
         (completion-regexp-list (and regexp (list regexp)))
         (case-fold-search completion-ignore-case))
    (all-completions "" obarray predicate)))

(defun omnibox--title (&optional command)
  (--> (or command this-command "?")
       (if (symbolp it) (symbol-name it) it)
       (replace-regexp-in-string "^omnibox-" "" it)
       (format " Omnibox-%s " it)))

(defun omnibox-M-x nil
  (interactive)
  (omnibox--set title (omnibox--title))
  (omnibox :prompt "M-x: "
           :candidates (lambda (input) (omnibox--obarray-candidates input 'commandp))
           :detail 'omnibox-M-x--doc))

(defun omnibox-describe-function nil
  (interactive)
  (omnibox--set title (omnibox--title))
  (omnibox :prompt "M-x: "
           :candidates (lambda (input) (omnibox--obarray-candidates input 'functionp))
           :detail 'omnibox-M-x--doc))

;; (omnibox :detail "moi" :candidates '("seb" "ok" "coucou") :prompt "seb: ")

(defun omnibox--make-frame (buffer)
  (-if-let* ((frame (omnibox--get frame)))
      (progn
        (omnibox--update-input-buffer)
        (make-frame-visible frame)
        (redisplay))
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
      (omnibox--set frame frame)
      (with-selected-frame frame
        (display-buffer-in-side-window
         (omnibox--update-input-buffer)
         '((side . top) (window-height . 1))))
      frame)))

(defun omnibox--candidate-at-point nil
  (buffer-substring (line-beginning-position)
                    (line-end-position)))

(defun omnibox--update-overlay nil
  "."
  (let ((documentation (or (get-text-property (point) 'omnibox-doc)
                           (-some--> (omnibox--get detail)
                                     (and (functionp it) it)
                                     (funcall it (omnibox--candidate-at-point)))
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
  (if (= (omnibox--get candidates-length) 0)
      (omnibox--disable-overlays)
    (omnibox--update-overlay)))

(defun omnibox--select nil
  (interactive)
  (with-current-buffer (omnibox--buffer)
    (omnibox--set selected (omnibox--candidate-at-point)))
  (omnibox--abort))

(defun omnibox--change-line (selection)
  (with-selected-window (get-buffer-window (omnibox--buffer) t)
    (omnibox--update-line selection)))

(defun omnibox--next nil
  (interactive)
  (setq omnibox-selection (min (1+ omnibox-selection)
                               (1- (omnibox--get candidates-length))))
  (omnibox--change-line omnibox-selection))

(defun omnibox--prev nil
  (interactive)
  (setq omnibox-selection (max (1- omnibox-selection) 0))
  (omnibox--change-line omnibox-selection))

(defun omnibox--hide nil
  (-some-> (omnibox--get frame)
           (make-frame-invisible)))

(defun omnibox--abort nil
  (interactive)
  (when (minibufferp)
    (exit-minibuffer))
  (omnibox-mode -1)
  (omnibox--hide)
  (omnibox--set input nil))

(defun omnibox--update-input (new-input)
  (omnibox--set input new-input)
  (omnibox--update-input-buffer new-input)
  (omnibox--update-list-buffer))

(defun omnibox--insert nil
  (interactive)
  (let* ((char (char-to-string last-command-event))
         (current (or (omnibox--get input) ""))
         (new-string (concat current char)))
    (omnibox--update-input new-string)))

(defun omnibox--backward-delete nil
  (interactive)
  (-when-let* ((current (omnibox--get input))
               (len (length current))
               (new-string (substring current 0 (max (1- len) 0))))
    (omnibox--update-input new-string)))

(defun omnibox--try-complete nil
  (interactive)
  (-some--> (omnibox--get input)
            (try-completion it (omnibox--fetch-candidates it))
            (substring-no-properties it)
            (omnibox--update-input it)))

(defun omnibox--make-char-table nil
  "."
  (let ((my-char-table (make-char-table 'my-char-table)))
    (map-char-table (lambda (key val)
                      (when (eq 'self-insert-command val)
                        (set-char-table-range my-char-table key 'omnibox--insert)))
                    (car (cdr global-map)))
    my-char-table))

(defvar omnibox-mode-map nil
  "Keymap for ‘omnibox-mode’.")
(unless omnibox-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\e\e" 'omnibox--abort)
    (define-key map "\C-g" 'omnibox--abort)
    (define-key map (kbd "C-n") 'omnibox--next)
    (define-key map (kbd "<down>") 'omnibox--next)
    (define-key map (kbd "C-p") 'omnibox--prev)
    (define-key map (kbd "<up>") 'omnibox--prev)
    (define-key map (kbd "RET") 'omnibox--select)
    (define-key map (kbd "<return>") 'omnibox--select)
    (define-key map (kbd "TAB") 'omnibox--try-complete)
    (define-key map (kbd "<tab>") 'omnibox--try-complete)
    (define-key map (kbd "DEL") 'omnibox--backward-delete)
    (define-key map (kbd "<backspace>") 'omnibox--backward-delete)
    (define-key map [t] 'ignore)
    (setq map (-snoc map (omnibox--make-char-table)))
    (setq omnibox-mode-map map)))

(define-minor-mode omnibox-mode
  "Mode for omnibox."
  :init-value nil)

(defun omnibox--completing-read (prompt collection &optional
                                        predicate require-match
                                        initial-input hist def
                                        inherit-input-method)
  (let ((omnibox--extern t))
    (omnibox--set predicate predicate)
    (omnibox--set title (format " Omnibox-%s " this-command))
    (omnibox :prompt prompt
             :candidates collection)
    (unwind-protect
        (read-from-minibuffer "" nil omnibox-mode-map)
      (omnibox--abort)
      (when (eq this-command 'omnibox--abort)
        (keyboard-quit)))
    (omnibox--get selected)))

(global-set-key (kbd "C-o") 'omnibox-M-x)
;;(setq completing-read-function 'omnibox--completing-read)

(provide 'omnibox)
;;; omnibox.el ends here
