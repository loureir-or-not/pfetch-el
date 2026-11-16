;;; pfetch.el --- pfetch clone for GNU Emacs -*- lexical-binding: t -*-

;; Author: Renato Loureiro
;; Maintainer: Renato Loureiro
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: https://github.com/loureir-or-not/pfetch.el
;; Keywords: fetch


;; This file is not part of GNU Emacs

;; (C) 2026 Renato Loureiro
;; 
;; This software is provided 'as-is', without any express or implied
;; warranty. In no event will the authors be held liable for any damages
;; arising from the use of this software.
;; 
;; Permission is granted to anyone to use this software for any purpose,
;; including commercial applications, and to alter it and redistribute it
;; freely, subject to the following restrictions:
;; 
;; 1. The origin of this software must not be misrepresented; you must not
;;    claim that you wrote the original software. If you use this software
;;    in a product, an acknowledgment in the product documentation would be
;;    appreciated but is not required.
;; 
;; 2. Altered source versions must be plainly marked as such, and must not be
;;    misrepresented as being the original software.
;; 
;; 3. This notice may not be removed or altered from any source distribution.


;;; Commentary:

;; pfetch-el is a clone of dylanaraps' pfetch for GNU Emacs. It is primary developed on Linux
;; systems.

;;; Code:

(defun pfetch--get-os-linux ()
  "Get the OS based off of /etc/os-release."
  (with-temp-buffer
    (insert-file-contents-literally "/etc/os-release")
    (search-forward "PRETTY_NAME=\"")
    (zap-up-to-char 1 ?\")
    (string-trim (nth 0 kill-ring))))

(defun pfetch--get-os ()
  "Fetch the running system's operating system."
  (pcase system-type
    ('gnu/linux (pfetch--get-os-linux))
    (_ (error "This does not support anything other than gnu/linux at the moment."))))

(defun pfetch--get-host-linux ()
  "Fetch information about the running Linux computer."
  (with-temp-buffer
    (insert-file-contents-literally "/sys/devices/virtual/dmi/id/product_name")
    (move-end-of-line nil)
    (insert " (")
    (insert-file-contents-literally "/sys/devices/virtual/dmi/id/product_family")
    (move-end-of-line nil)
    (insert ")")
    (string-trim (buffer-string))))

(defun pfetch--get-host ()
  "Fetch information about the running computer."
  (pcase system-type
    ('gnu/linux (pfetch--get-host-linux))
    (_ (error "This does not support anything other than gnu/linux at the moment."))))

(defun pfetch--get-kernel-linux ()
  "Fetch information about the running Linux kernel."
  (with-temp-buffer
    (insert-file-contents-literally "/proc/version")
    (search-forward "version")
    (zap-to-char -1 ?\s)
    (forward-whitespace 2)
    (kill-line)
    (string-trim (buffer-string))))

(defun pfetch--get-kernel ()
  "Fetch information about the running kernel."
  (pcase system-type
    ('gnu/linux (pfetch--get-kernel-linux))
    (_ (error "This does not support anything other than gnu/linux at the moment."))))

(defun pfetch--get-memory-linux ()
  "Fetch information about the memory of the running Linux system."
  (with-temp-buffer
    (insert-file-contents-literally "/proc/meminfo")
    (let* ((lines (split-string (string-trim (buffer-string)) "\n"))
           (pairs '()))
      (dolist (line lines)
        (setq pair (split-string line ":")
              key (nth 0 pair)
              val (string-trim (nth 1 pair)))
        (push (cons key val) pairs))
      pairs)))

(defun pfetch--get-memory-total-linux (meminfo)
  "Find the total memory of the Linux system from `meminfo'."
  (let ((result 0))
    (dolist (pair meminfo result)
      (pcase pair
        (`("MemTotal" ,x) (setq result (string-to-number x)))
        (_ '())))))

(defun pfetch--get-memory-available-linux (meminfo)
  "Find the available memory of the Linux system from `meminfo'."
  (let ((result 0))
    (dolist (pair meminfo result)
      (pcase pair
        (`("MemAvailable" ,x) (setq result (string-to-number x)))
        (_ '())))))

(defun pfetch--format-memory-linux (meminfo)
  "Format a string displaying the memory of the Linux system from `meminfo'."
  (let ((total-memory (pfetch--get-memory-available-linux meminfo))
        (available-memory (pfetch--get-memory-available-linux meminfo)))
    (format "%dMB / %dMB"
            (truncate (/ total-memory 1000))
            (truncate (/ available-memory 1000)))))

(defun pfetch--get-uptime-linux ()
  "Get the uptime of the running Linux system."
  (with-temp-buffer
    (insert-file-contents-literally "/proc/uptime")
    (zap-up-to-char 1 ?\s)
    (truncate (string-to-number (nth 0 kill-ring)))))

(defun pfetch--format-uptime-linux (uptime)
  "Format a string of the uptime for the running Linux system."
  (let ((minutes (% (/ uptime 60) 60))
        (hours (% (/ uptime 3600) 24))
        (days (/ (/ uptime 3600) 24)))
    (format "%dd %dh %dm" days hours minutes)))

(defun pfetch--get-uptime ()
  "Get the uptime for the running system."
  (pcase system-type
    ('gnu/linux (pfetch--get-uptime-linux))
    (_ (error "This does not support anything other than gnu/linux at the moment."))))

(defun pfetch--format-uptime (uptime)
  "Format a string for the uptime of the running system."
  (pcase system-type
    ('gnu/linux (pfetch--format-uptime-linux uptime))
    (_ (error "This does not support anything other than gnu/linux at the moment."))))

(defun pfetch--get-shell-unix ()
  "Get the user's shell for UNIX systems."
  (file-name-nondirectory (getenv "SHELL")))

(defun pfetch--get-shell ()
  "Get the user's shell."
  (pcase system-type
    ('gnu/linux (pfetch--get-shell-unix))
    (_ (error "This does not support anything other than gnu/linux at the moment."))))

(defun pfetch--get-memory ()
  "Get the memory of the running system."
  (pcase system-type
    ('gnu/linux (pfetch--get-memory-linux))
    (_ (error "This does not support anything other than gnu/linux at the moment."))))

(defun pfetch--format-memory (meminfo)
  "Format a string displaying the memory of the system from `meminfo'"
  (pcase system-type
    ('gnu/linux (pfetch--format-memory-linux meminfo))
    (_ (error "This does not support anything other than gnu/linux at the moment."))))

(defun pfetch ()
  "Fetch the running system's information."
  (interactive)
  (let* ((buffer-name (format "*pfetch.el<%s>*" current-prefix-arg))
         (buffer (generate-new-buffer buffer-name))
         (memory (pfetch--get-memory))
         (uptime (pfetch--get-uptime)))
    (with-current-buffer buffer
      (insert (format "%s@%s\n" user-login-name system-name))
      (insert (format "os\t%s\n" (pfetch--get-os)))
      (insert (format "host\t%s\n" (pfetch--get-host)))
      (insert (format "kernel\t%s\n" (pfetch--get-kernel)))
      (insert (format "uptime\t%s\n" (pfetch--format-uptime uptime)))
      (insert (format "shell\t%s\n" (pfetch--get-shell)))
      (insert (format "memory\t%s\n" (pfetch--format-memory memory))))
    (switch-to-buffer buffer-name)
    (read-only-mode t)))

(provide 'pfetch)

;;; pfetch.el ends here
