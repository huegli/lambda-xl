;;;; Load script for LispWorks Atari Emulator
;;;; This file can be used with LispWorks -init option

(in-package :cl-user)

;; Load the emulator
(load (merge-pathnames "atari-emulator.lisp" *load-pathname*))

;; Print instructions
(format t "~%~%")
(format t "========================================~%")
(format t "Atari 800 Emulator - LispWorks CAPI~%")
(format t "========================================~%")
(format t "~%")
(format t "To run the emulator, execute:~%")
(format t "  (atari-emulator:run-emulator)~%")
(format t "~%")
(format t "Or to run immediately:~%")
(format t "  (in-package :atari-emulator)~%")
(format t "  (run-emulator)~%")
(format t "~%")
(format t "========================================~%")
(format t "~%")
