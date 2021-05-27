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

;;; Commentary:

;; 

;;; Code:

;; (setq tagbar-name "tagbar")

;; (get-buffer-create tagbar-name)
;; (switch-to-buffer tagbar-name)
;; buffer-file-name

(setq function_position nil)
(setq target_buffer (buffer-name))
(setq tagbar_buffer nil)

(defun what-line ()
  "Print the current line number (in the buffer) of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
	    (1+ (count-lines 1 (point))))))

(defun process_tagbar()
  (interactive)
  (setq function_position nil)
  (kill-whole-line)
  (kill-whole-line)
  (save-excursion
    (progn
	(goto-char (point-max))
	(setq tagbar/linenumbers (what-line))))
  (while ( < (what-line) tagbar/linenumbers)
    (progn 
	(end-of-line)
	(search-backward ",")
	(backward-char)
	(getpoint)
	(search-backward ")")
	(forward-char)
	(kill-line)
	(forward-line 1))))

(defun test()
    (interactive)
    (if tagbar_buffer
	(progn
	  (message "Buffer exists")
	  (switch-to-buffer tagbar_buffer))
        (progn
	  (setq target_buffer (buffer-name))
	  (setq filename buffer-file-name)
	  (setq command-string (concat "ctags -e " buffer-file-name))
	  (shell-command command-string)
	  (get-buffer-create "tagbar")
	  (switch-to-buffer "tagbar")
	  (setq tagbar_buffer (buffer-name))
	  (insert-file-contents "TAGS")
	  (process_tagbar))))

(defun search_copyight()
  (interactive)
  (search-forward ""))

(defun getpoint()
  (interactive)
  (let* ((line (what-line)) (position (current-word))) 
    (push (cons line position) function_position)))

(defun tagbar/gotodefinition()
  (interactive)
  (let* ((line (what-line)) (list function_position))
    (while (not (eq (car (car list)) line))
      (setq list (cdr list)))
    (message (number-to-string (car (car list))))
    (setq target_line (string-to-number (cdr (car list))))
    (if (integerp target_line)
	(message "y"))
    (switch-to-buffer target_buffer)
    (goto-line target_line)))
    ;; (while (not (eq line (string-to-number (car (car (list))))))
    ;;   (message (car (car list)))
    ;;   (setq list (cdr list)))
    ;; (switch-to-buffer target_buffer)
    ;; (goto-line target)))

(define-derived-mode tagbar-mode text-mode "Tagbar"
  "Mode for navigating definition"
  (setq function_position nil)
  (modern-c++-font-lock-mode 1)
  (message "Welcome to tagbar"))

(defvar tagbar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-f" 'tagbar/gotodefinition)
    map))


;; (defvar tagbar-mode-syntax-table
;;   (funcall (c-lang-const c-make-mode-syntax-table c++))
;;   "Syntax table used in c++-mode buffers.")

asdddddddddddadsssssssa
;;; tagbar.el ends here
