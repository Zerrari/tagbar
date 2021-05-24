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
    (setq filename buffer-file-name)
    (setq command-string (concat "ctags -e " buffer-file-name))
    (shell-command command-string)
    (get-buffer-create "tagber")
    (switch-to-buffer "tagbar")
    (insert-file-contents "TAGS")
    (process_tagbar))

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
    (while (not (eq line (car (car (list)))))
      (setq list (cdr list)))
    (setq target ((string-to-numbercdr (car list))))
    (switch-to-buffer target_buffer)
    (goto-line target)))

(define-derived-mode tagbar-mode text-mode "Tagbar"
  "Mode for navigating definition"
  (setq function_position nil))

(defvar tagbar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-f" 'tagbar/goto-definition)
    map))

asdddddddddddadsssssssa
;;; tagbar.el ends here
