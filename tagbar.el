;;; tagbar.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  zerrari

;; Author: zerrari <zerrari@zhangyizhongdeMacBook-Pro.local>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Commentary:

;; 

;;; Code:

(defvar tagbar-target-buffer nil
  "Keep the target buffer name in this variable.")

(defvar tagbar-mode-buffer nil
  "Keep the mode-buffer-name in this variable.")

(setq tagbar-mode-buffer nil)

(defvar tagbar-function-position-list nil
  "Keep the functions and their positions in this variable.")

(defvar tagbar-check-buffer-exists nil
  "This variable checks if the tagbar buffer exists.")

(defvar tagbar-current-line-number nil
  "Store the current line number in tagbar buffer.")

;; (defvar tagbar-current-file-name nil
;;   "Store the current file name.")

(defvar tagbar-ctags-command-string nil
  ;;(concat "ctags -e " buffer-file-name)
  "Store the shell command to create TAGS file.")

(defvar tagbar-target-line nil
  "Store the line which the definition locates.")

(defvar tagbar-mode-hook nil)

(defvar tagbar-mode-map nil)

(defvar tagbar-syntax-table c-mode-syntax-table
  "Set the variable to c-mode-syntax-table.")

(defun tagbar-what-line ()
  "Print the current line number (in the buffer) of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
	    (1+ (count-lines 1 (point))))))

(defun tagbar-process-tags()
  "Process TAGS file."
  (interactive)
  (setq tagbar-function-position-list nil)
  (kill-whole-line)
  (kill-whole-line)
  (save-excursion
    (progn
	(goto-char (point-max))
	(setq tagbar-current-line-number (tagbar-what-line))))
  (while ( < (tagbar-what-line) tagbar-current-line-number)
    (progn 
	(end-of-line)
	(search-backward ",")
	(backward-char)
	(tagbar-get-position)
	(search-backward ")")
	(forward-char)
	(kill-line)
	(beginning-of-line)
	(search-forward " ")
	(backward-kill-word 1)
	(end-of-line)
	(insert ";")
	(forward-line 1))))

(defun tagbar-toggle()
  "Toggle tagbar."
    (interactive)
    (if tagbar-mode-buffer
	(progn
	  (message "Buffer exists")
	  (switch-to-buffer tagbar-mode-buffer))
        (progn
	  (setq tagbar-target-buffer (buffer-name))
	  (setq tagbar-ctags-command-string (concat "ctags -e " buffer-file-name))
	  (shell-command tagbar-ctags-command-string)
	  (get-buffer-create "tagbar")
	  (split-window-right)
	  (other-window 1)
	  (switch-to-buffer "tagbar")
	  (tagbar-mode)
	  (setq tagbar-mode-buffer (buffer-name))
	  (erase-buffer)
	  (insert-file-contents "TAGS")
	  (tagbar-process-tags)
	  (forward-line (- 1 (tagbar-what-line)))
	  (insert "  Functions:\n")
	  (setq buffer-read-only t))))

(defun tagbar-quit ()
  "Quit the tagbar buffer."
  (interactive)
  (setq tagbar-mode-buffer nil)
  (kill-buffer-and-window))
 

(defun tagbar-disable-evil-key ()
  "Disable some evil keys in tagbar buffer."
  (evil-local-set-key 'normal (kbd "f") 'tagbar-goto-definition)
  (evil-local-set-key 'normal (kbd "q") 'tagbar-quit))

(defun tagbar-get-position()
  "Get currnet position to form list."
  (interactive)
  (let* ((line (tagbar-what-line)) (position (current-word))) 
    (push (cons (+ 1 line) position) tagbar-function-position-list)))

(defun tagbar-goto-definition()
  "Go to the function definition."
  (interactive)
  (let* ((line (tagbar-what-line)) (list tagbar-function-position-list))
    (while (not (eq (car (car list)) (+ 1 line)))
      (setq list (cdr list)))
    (message (number-to-string (car (car list))))
    (setq tagbar-target-line (string-to-number (cdr (car list))))
    (if (integerp tagbar-target-line)
	(message "y"))
    (other-window 1)
    ;; (switch-to-buffer tagbar-target-buffer)
    ;;(goto-line target_line)
    (forward-line (- tagbar-target-line (tagbar-what-line)))))
    ;; (while (not (eq line (string-to-number (car (car (list))))))
    ;;   (message (car (car list)))
    ;;   (setq list (cdr list)))
    ;; (switch-to-buffer target_buffer)
    ;; (goto-line target)))


(progn
  (setq tagbar-mode-map (make-sparse-keymap))
  (define-key tagbar-mode-map (kbd "C-q") 'tagbar-quit)
  (message "OK")
  (define-key tagbar-mode-map (kbd "C-f") 'tagbar-goto-definition))

;;;###autoload
(add-to-list 'auto-mode-alist '("tagbar" . tagbar-mode))

(define-derived-mode tagbar-mode c-mode
  "Mode for navigating definition"
  (setq tagbar-function-position-list nil)
  (setq tagbar-mode-buffer nil)
  (use-local-map tagbar-mode-map) 
  (tagbar-disable-evil-key)
  (message "Welcome to tagbar"))


;;; tagbar.el ends here
