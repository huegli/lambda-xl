;;;; Enhanced Atari 800 Emulator using LispWorks CAPI and FLI
;;;; This version includes keyboard input handling

(in-package :cl-user)

(defpackage :atari-emulator
  (:use :cl :capi :fli)
  (:export #:run-emulator))

(in-package :atari-emulator)

;;;; FLI Definitions for libatari800

;; Basic type definitions
(fli:define-c-typedef ubyte (:unsigned :char))
(fli:define-c-typedef uword (:unsigned :short))
(fli:define-c-typedef ulong :uint32)

;; Atari key codes
(defconstant +akey-escape+ #x1c)
(defconstant +akey-space+ #x21)
(defconstant +akey-return+ #x0c)
(defconstant +akey-backspace+ #x34)
(defconstant +akey-up+ #x8e)
(defconstant +akey-down+ #x8f)
(defconstant +akey-left+ #x86)
(defconstant +akey-right+ #x87)
(defconstant +akey-tab+ #x2c)

;; Input template structure
(fli:define-c-struct input-template-t
  (keychar ubyte)
  (keycode ubyte)
  (special ubyte)
  (shift ubyte)
  (control ubyte)
  (start ubyte)
  (select ubyte)
  (option ubyte)
  (joy0 ubyte)
  (trig0 ubyte)
  (joy1 ubyte)
  (trig1 ubyte)
  (joy2 ubyte)
  (trig2 ubyte)
  (joy3 ubyte)
  (trig3 ubyte)
  (mousex ubyte)
  (mousey ubyte)
  (mouse-buttons ubyte)
  (mouse-mode ubyte))

;; State save tags structure
(fli:define-c-struct statesav-tags-t
  (size ulong)
  (cpu ulong)
  (pc ulong)
  (base-ram ulong)
  (base-ram-attrib ulong)
  (antic ulong)
  (gtia ulong)
  (pia ulong)
  (pokey ulong))

;; State save flags structure
(fli:define-c-struct statesav-flags-t
  (selftest-enabled ubyte)
  (align1 (:c-array ubyte 3))
  (nframes ulong)
  (sample-residual ulong))

;; Emulator state structure (simplified for access)
(fli:define-c-struct emulator-state-t
  (tags-storage (:c-array ubyte 128))
  (flags-storage (:c-array ubyte 128))
  (state (:c-array ubyte 210000)))

;; CPU state structure
(fli:define-c-struct cpu-state-t
  (a ubyte)
  (p ubyte)
  (s ubyte)
  (x ubyte)
  (y ubyte)
  (irq ubyte))

;; PC state structure
(fli:define-c-struct pc-state-t
  (pc uword))

;; Foreign function definitions
(fli:define-foreign-function (libatari800-init "libatari800_init" :source)
    ((argc :int)
     (argv (:pointer (:pointer :char))))
  :result-type :int
  :module :libatari800)

(fli:define-foreign-function (libatari800-error-message "libatari800_error_message" :source)
    ()
  :result-type (:pointer :char)
  :module :libatari800)

(fli:define-foreign-function (libatari800-clear-input-array "libatari800_clear_input_array" :source)
    ((input (:pointer (:struct input-template-t))))
  :result-type :void
  :module :libatari800)

(fli:define-foreign-function (libatari800-next-frame "libatari800_next_frame" :source)
    ((input (:pointer (:struct input-template-t))))
  :result-type :int
  :module :libatari800)

(fli:define-foreign-function (libatari800-get-screen-ptr "libatari800_get_screen_ptr" :source)
    ()
  :result-type (:pointer ubyte)
  :module :libatari800)

(fli:define-foreign-function (libatari800-get-fps "libatari800_get_fps" :source)
    ()
  :result-type :float
  :module :libatari800)

(fli:define-foreign-function (libatari800-get-frame-number "libatari800_get_frame_number" :source)
    ()
  :result-type :int
  :module :libatari800)

(fli:define-foreign-function (libatari800-get-current-state "libatari800_get_current_state" :source)
    ((state (:pointer (:struct emulator-state-t))))
  :result-type :void
  :module :libatari800)

(fli:define-foreign-function (libatari800-get-sound-frequency "libatari800_get_sound_frequency" :source)
    ()
  :result-type :int
  :module :libatari800)

(fli:define-foreign-function (libatari800-get-sound-sample-size "libatari800_get_sound_sample_size" :source)
    ()
  :result-type :int
  :module :libatari800)

(fli:define-foreign-function (libatari800-get-num-sound-channels "libatari800_get_num_sound_channels" :source)
    ()
  :result-type :int
  :module :libatari800)

(fli:define-foreign-function (libatari800-get-sound-buffer-allocated-size "libatari800_get_sound_buffer_allocated_size" :source)
    ()
  :result-type :int
  :module :libatari800)

(fli:define-foreign-function (libatari800-exit "libatari800_exit" :source)
    ()
  :result-type :void
  :module :libatari800)

;; Bridge helper functions (if available)
(fli:define-foreign-function (get-cpu-state "get_cpu_state" :source)
    ((state (:pointer (:struct emulator-state-t))))
  :result-type (:pointer (:struct cpu-state-t))
  :module :bridge
  :no-check t) ; Don't error if not found

(fli:define-foreign-function (get-pc-state "get_pc_state" :source)
    ((state (:pointer (:struct emulator-state-t))))
  :result-type (:pointer (:struct pc-state-t))
  :module :bridge
  :no-check t) ; Don't error if not found

;;;; Emulator Constants

(defconstant +screen-width+ 384)
(defconstant +screen-height+ 240)
(defconstant +screen-size+ (* +screen-width+ +screen-height+))
(defconstant +display-scale+ 2)
(defconstant +target-fps+ 60.0)
(defconstant +frame-delay+ (floor (/ 1000.0 +target-fps+))) ; milliseconds

;;;; Color Conversion Functions

(defun atari-byte-to-rgb (pixel-byte)
  "Convert Atari pixel byte to RGB values (0-255 range)"
  (let* ((hue (logand (ash pixel-byte -4) #x0F))
         (luminance (logand pixel-byte #x0F))
         (h (/ hue 15.0))
         (s 0.8)
         (v (/ luminance 15.0))
         ;; HSV to RGB conversion
         (c (* v s))
         (x (* c (- 1.0 (abs (- (mod (* h 6.0) 2.0) 1.0)))))
         (m (- v c))
         (h-prime (* h 6.0)))
    (multiple-value-bind (r-prime g-prime b-prime)
        (cond ((< h-prime 1.0) (values c x 0.0))
              ((< h-prime 2.0) (values x c 0.0))
              ((< h-prime 3.0) (values 0.0 c x))
              ((< h-prime 4.0) (values 0.0 x c))
              ((< h-prime 5.0) (values x 0.0 c))
              (t (values c 0.0 x)))
      (values (floor (* (+ r-prime m) 255))
              (floor (* (+ g-prime m) 255))
              (floor (* (+ b-prime m) 255))))))

(defun create-color-lookup-table ()
  "Create a 256-entry color lookup table for Atari palette"
  (let ((table (make-array 256)))
    (dotimes (i 256)
      (multiple-value-bind (r g b)
          (atari-byte-to-rgb i)
        (setf (aref table i) (color:make-rgb r g b :scale 255))))
    table))

;;;; Keyboard Mapping

(defun map-key-to-atari (key-spec)
  "Map LispWorks key specification to Atari keycode"
  (cond
    ;; Special keys
    ((eq key-spec :escape) +akey-escape+)
    ((eq key-spec :return) +akey-return+)
    ((eq key-spec :space) +akey-space+)
    ((eq key-spec :backspace) +akey-backspace+)
    ((eq key-spec :up) +akey-up+)
    ((eq key-spec :down) +akey-down+)
    ((eq key-spec :left) +akey-left+)
    ((eq key-spec :right) +akey-right+)
    ((eq key-spec :tab) +akey-tab+)
    ;; Character keys - return char code for keychar field
    ((characterp key-spec) (char-code (char-upcase key-spec)))
    ;; Default
    (t nil)))

;;;; Emulator State Class

(defclass emulator-state ()
  ((running :initform nil :accessor emulator-running-p)
   (input-struct :initform nil :accessor emulator-input-struct)
   (state-struct :initform nil :accessor emulator-state-struct)
   (screen-buffer :initform (make-array +screen-size+ :element-type '(unsigned-byte 8) :initial-element 0)
                  :accessor emulator-screen-buffer)
   (color-table :initform (create-color-lookup-table) :accessor emulator-color-table)
   (frame-count :initform 0 :accessor emulator-frame-count)
   (last-frame-time :initform 0 :accessor emulator-last-frame-time)
   (emulation-thread :initform nil :accessor emulator-thread)
   (max-frames :initform nil :accessor emulator-max-frames) ; nil = run forever
   (key-lock :initform (mp:make-lock) :accessor emulator-key-lock)
   (pending-key :initform nil :accessor emulator-pending-key)))

;;;; CAPI Display Canvas with optimized rendering

(define-interface atari-display-interface ()
  ((emulator-state :initform (make-instance 'emulator-state) :accessor interface-emulator-state)
   (cached-image :initform nil :accessor interface-cached-image))
  (:panes
   (canvas output-pane
           :display-callback 'draw-atari-screen
           :resize-callback 'canvas-resized
           :input-model '(((:button-1 :press) handle-mouse-click)
                         ((:key-press :meta-:up) handle-key-up)
                         (:key-press handle-key-press))
           :visible-min-width (* +screen-width+ +display-scale+)
           :visible-min-height (* +screen-height+ +display-scale+)
           :background :black)
   (status-text title-pane
                :text "Atari 800 Emulator - LispWorks CAPI - Press any key to send to emulator"))
  (:layouts
   (main-layout column-layout '(status-text canvas)))
  (:menus
   (application-menu "File"
    (("Quit" :callback 'quit-interface
             :accelerator "accelerator-q"))))
  (:menu-bar application-menu)
  (:default-initargs
   :title "Atari 800 Emulator"
   :best-width (* +screen-width+ +display-scale+)
   :best-height (+ (* +screen-height+ +display-scale+) 30)
   :destroy-callback 'cleanup-emulator))

(defun draw-atari-screen (pane x y width height)
  "Draw the Atari screen buffer to the CAPI canvas using optimized rendering"
  (declare (ignore x y width height))
  (let* ((interface (capi:element-interface pane))
         (emulator (interface-emulator-state interface))
         (buffer (emulator-screen-buffer emulator))
         (color-table (emulator-color-table emulator))
         (pane-width (gp:port-width pane))
         (pane-height (gp:port-height pane))
         (scale-x (/ pane-width +screen-width+))
         (scale-y (/ pane-height +screen-height+)))

    ;; For better performance, we could use an external image and blit it
    ;; For now, direct pixel drawing with proper scaling
    (when (and (> scale-x 0) (> scale-y 0))
      (dotimes (y +screen-height+)
        (dotimes (x +screen-width+)
          (let* ((index (+ (* y +screen-width+) x))
                 (pixel-byte (aref buffer index))
                 (color (aref color-table pixel-byte))
                 (draw-x (floor (* x scale-x)))
                 (draw-y (floor (* y scale-y)))
                 (draw-w (max 1 (ceiling scale-x)))
                 (draw-h (max 1 (ceiling scale-y))))
            (gp:draw-rectangle pane draw-x draw-y draw-w draw-h
                              :filled t
                              :foreground color)))))))

(defun canvas-resized (pane x y width height)
  "Handle canvas resize events"
  (declare (ignore x y width height))
  (gp:invalidate-rectangle pane))

(defun handle-mouse-click (pane x y)
  "Handle mouse clicks on the canvas"
  (declare (ignore pane x y))
  nil)

(defun handle-key-press (pane key)
  "Handle keyboard input and send to emulator"
  (let* ((interface (capi:element-interface pane))
         (emulator (interface-emulator-state interface)))
    (when (emulator-running-p emulator)
      (let ((atari-key (map-key-to-atari key)))
        (when atari-key
          (mp:with-lock ((emulator-key-lock emulator))
            (setf (emulator-pending-key emulator) atari-key))
          (format t "Key pressed: ~A -> Atari code: ~A~%" key atari-key))))))

(defun handle-key-up (pane key)
  "Handle key release"
  (declare (ignore pane key))
  ;; Could clear key state here
  nil)

(defun quit-interface (interface)
  "Quit the application"
  (capi:destroy interface))

;;;; Emulator Initialization and Control

(defun initialize-libatari800 ()
  "Initialize the libatari800 library"
  (let* ((args (list "-atari"))
         (argc (length args))
         (argv (fli:allocate-foreign-object :type '(:pointer :char)
                                            :nelems (1+ argc))))
    (unwind-protect
        (progn
          ;; Set up argv array
          (loop for arg in args
                for i from 0
                do (setf (fli:dereference argv :index i)
                         (fli:convert-to-foreign-string arg)))
          ;; NULL terminator
          (setf (fli:dereference argv :index argc) fli:*null-pointer*)

          ;; Call init
          (let ((result (libatari800-init argc argv)))
            (when (/= result 0)
              (format t "libatari800 initialization returned code: ~A~%" result)
              (let ((error-msg (libatari800-error-message)))
                (unless (fli:null-pointer-p error-msg)
                  (format t "Error message: ~A~%" (fli:convert-from-foreign-string error-msg))))
              (format t "Proceeding anyway to see if emulation works...~%"))

            ;; Print emulator info
            (format t "emulation: fps=~A~%" (libatari800-get-fps))
            (format t "sound: freq=~A, bytes/sample=~A, channels=~A, max buffer size=~A~%"
                    (libatari800-get-sound-frequency)
                    (libatari800-get-sound-sample-size)
                    (libatari800-get-num-sound-channels)
                    (libatari800-get-sound-buffer-allocated-size))
            result))
      ;; Cleanup
      (loop for i from 0 below argc
            do (fli:free-foreign-object (fli:dereference argv :index i)))
      (fli:free-foreign-object argv))))

(defun start-emulator (interface &key (max-frames nil))
  "Start the emulator thread"
  (let ((emulator (interface-emulator-state interface)))
    (when (emulator-running-p emulator)
      (return-from start-emulator nil))

    ;; Set max frames
    (setf (emulator-max-frames emulator) max-frames)

    ;; Allocate FLI structures
    (setf (emulator-input-struct emulator)
          (fli:allocate-foreign-object :type '(:struct input-template-t)))
    (setf (emulator-state-struct emulator)
          (fli:allocate-foreign-object :type '(:struct emulator-state-t)))

    ;; Initialize libatari800
    (initialize-libatari800)

    ;; Clear input array
    (libatari800-clear-input-array (emulator-input-struct emulator))

    ;; Start emulation
    (setf (emulator-running-p emulator) t)
    (setf (emulator-last-frame-time emulator) (get-internal-real-time))

    ;; Start emulation thread
    (setf (emulator-thread emulator)
          (mp:process-run-function "Atari Emulator"
                                   nil
                                   #'emulation-loop
                                   interface))))

(defun stop-emulator (emulator)
  "Stop the emulator"
  (when (emulator-running-p emulator)
    (setf (emulator-running-p emulator) nil)

    ;; Wait for thread to finish
    (when (emulator-thread emulator)
      (mp:process-wait "Waiting for emulator to stop"
                       (lambda () (not (mp:process-alive-p (emulator-thread emulator)))))
      (setf (emulator-thread emulator) nil))

    ;; Cleanup libatari800
    (libatari800-exit)

    ;; Free FLI structures
    (when (emulator-input-struct emulator)
      (fli:free-foreign-object (emulator-input-struct emulator))
      (setf (emulator-input-struct emulator) nil))
    (when (emulator-state-struct emulator)
      (fli:free-foreign-object (emulator-state-struct emulator))
      (setf (emulator-state-struct emulator) nil))))

(defun cleanup-emulator (interface)
  "Cleanup when interface is destroyed"
  (let ((emulator (interface-emulator-state interface)))
    (stop-emulator emulator)))

(defun update-frame (interface)
  "Update one emulation frame"
  (let* ((emulator (interface-emulator-state interface))
         (input (emulator-input-struct emulator))
         (state (emulator-state-struct emulator))
         (frame-start-time (get-internal-real-time))
         (time-since-last (- frame-start-time (emulator-last-frame-time emulator)))
         (start-time (get-internal-real-time)))

    (setf (emulator-last-frame-time emulator) frame-start-time)

    ;; Check for pending key input
    (mp:with-lock ((emulator-key-lock emulator))
      (when (emulator-pending-key emulator)
        (setf (fli:foreign-slot-value input 'keychar :object-type '(:struct input-template-t))
              (emulator-pending-key emulator))
        (setf (emulator-pending-key emulator) nil)))

    ;; Get current state (if bridge is available)
    (handler-case
        (progn
          (libatari800-get-current-state state)

          ;; Try to get CPU and PC state
          (let* ((cpu-ptr (get-cpu-state state))
                 (pc-ptr (get-pc-state state)))
            (unless (or (fli:null-pointer-p cpu-ptr) (fli:null-pointer-p pc-ptr))
              (let ((cpu-a (fli:foreign-slot-value cpu-ptr 'a :object-type '(:struct cpu-state-t)))
                    (cpu-x (fli:foreign-slot-value cpu-ptr 'x :object-type '(:struct cpu-state-t)))
                    (cpu-y (fli:foreign-slot-value cpu-ptr 'y :object-type '(:struct cpu-state-t)))
                    (cpu-s (fli:foreign-slot-value cpu-ptr 's :object-type '(:struct cpu-state-t)))
                    (cpu-p (fli:foreign-slot-value cpu-ptr 'p :object-type '(:struct cpu-state-t)))
                    (pc (fli:foreign-slot-value pc-ptr 'pc :object-type '(:struct pc-state-t))))

                (let* ((execution-time (/ (- (get-internal-real-time) start-time)
                                         (/ internal-time-units-per-second 1000.0)))
                       (time-since-ms (/ time-since-last
                                        (/ internal-time-units-per-second 1000.0))))
                  (format t "frame ~D: A=~2,'0X X=~2,'0X Y=~2,'0X SP=~2,'0X SR=~2,'0X PC=~4,'0X (one iteration took ~,3F ms, time since last frame: ~,3F ms)~%"
                          (libatari800-get-frame-number)
                          cpu-a cpu-x cpu-y cpu-s cpu-p pc
                          execution-time time-since-ms))))))
      (error (e)
        ;; If bridge functions aren't available, just continue
        (declare (ignore e))))

    ;; Process next frame
    (libatari800-next-frame input)

    ;; Clear input for next frame
    (setf (fli:foreign-slot-value input 'keychar :object-type '(:struct input-template-t)) 0)

    ;; Update screen buffer
    (update-screen-buffer emulator)

    (incf (emulator-frame-count emulator))

    ;; Stop if max frames reached
    (when (and (emulator-max-frames emulator)
               (>= (libatari800-get-frame-number) (emulator-max-frames emulator)))
      (setf (emulator-running-p emulator) nil))))

(defun update-screen-buffer (emulator)
  "Update the screen buffer from libatari800"
  (let ((screen-ptr (libatari800-get-screen-ptr)))
    (unless (fli:null-pointer-p screen-ptr)
      (let ((buffer (emulator-screen-buffer emulator)))
        (dotimes (i +screen-size+)
          (setf (aref buffer i) (fli:dereference screen-ptr :index i)))))))

(defun emulation-loop (interface)
  "Main emulation loop running in separate thread"
  (let ((emulator (interface-emulator-state interface)))
    (unwind-protect
        (loop while (emulator-running-p emulator)
              do (progn
                   (update-frame interface)

                   ;; Request redraw on main thread
                   (capi:execute-with-interface interface
                     (lambda ()
                       (let ((canvas (slot-value interface 'canvas)))
                         (gp:invalidate-rectangle canvas))))

                   ;; Sleep for target frame rate
                   (sleep (/ +frame-delay+ 1000.0))))
      ;; Cleanup
      (format t "Emulation loop exiting~%"))))

;;;; Main Entry Point

(defun load-libraries ()
  "Load the required dynamic libraries"
  (let ((lib-path (merge-pathnames "libatari800.dylib"
                                   (make-pathname :directory (pathname-directory
                                                              (or *load-pathname* *default-pathname-defaults*))))))
    ;; Try parent directory too
    (unless (probe-file lib-path)
      (setf lib-path (merge-pathnames "../libatari800.dylib"
                                      (make-pathname :directory (pathname-directory
                                                                (or *load-pathname* *default-pathname-defaults*))))))
    (unless (probe-file lib-path)
      (error "Cannot find libatari800.dylib at ~A or parent directory" lib-path))
    (format t "Loading libatari800 from: ~A~%" lib-path)
    (fli:register-module :libatari800
                        :real-name (namestring lib-path)
                        :connection-style :immediate))

  ;; Try to load bridge library if it exists
  (let ((bridge-paths (list
                       (merge-pathnames ".build/debug/libbridge.dylib"
                                       (make-pathname :directory (pathname-directory
                                                                 (or *load-pathname* *default-pathname-defaults*))))
                       (merge-pathnames "build/libbridge.dylib"
                                       (make-pathname :directory (pathname-directory
                                                                 (or *load-pathname* *default-pathname-defaults*)))))))
    (loop for bridge-path in bridge-paths
          when (probe-file bridge-path)
          do (progn
               (format t "Loading bridge from: ~A~%" bridge-path)
               (fli:register-module :bridge
                                   :real-name (namestring bridge-path)
                                   :connection-style :immediate)
               (return))
          finally (format t "Note: Bridge library not found, CPU state display will be limited~%"))))

(defun run-emulator (&key (max-frames nil))
  "Main entry point to run the Atari emulator
   max-frames: optional maximum number of frames to run (nil = run forever)"
  (load-libraries)
  (let ((interface (make-instance 'atari-display-interface)))
    (capi:display interface)
    ;; Start emulator after interface is displayed
    (capi:execute-with-interface interface
      (lambda ()
        (start-emulator interface :max-frames max-frames)))
    interface))

;; For standalone execution
(defun main ()
  "Entry point for standalone executable"
  (run-emulator)
  ;; Keep the main thread alive
  (loop (sleep 1)))
