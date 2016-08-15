;;;; qt-3d-fft.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:qt-3d-fft)
(named-readtables:in-readtable :qtools)

(declaim (otpimize (speed 3) (safety 1) (size 0) (debug 0)))

(defparameter *fps* 60)

(define-widget main-window (QMainWindow)
  ((mixer :initform (mixalot:create-mixer))
   (current-stream :initform nil)))

(define-override (main-window close-event) (ev)
  (mixalot:mixer-remove-all-streamers mixer)
  (mixalot:destroy-mixer mixer)
  (q+:accept ev))


(define-menu (main-window File)
  (:item ("Open" (ctrl o))
         (open-file main-window))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Interactively view and manipulate FFT data.")))


(define-widget fft-drawer (QGLWidget)
               ((the-mp3 :initform nil)
                (current-location :initform 0)
                (song-duration :initform 0)
                (total-frames :initform 0)
                (x-rot :initform 0))
  (:documentation "Draw "))


(define-subwidget (fft-drawer timer) (q+:make-qtimer fft-drawer)
  (setf (q+:single-shot timer) nil))

(define-initializer (fft-drawer setup)
  (q+:start timer (round (/ 1000 *fps*)))
  (setf (q+:auto-fill-background fft-drawer) nil))

(define-slot (fft-drawer tick) ()
  (declare (connected timer (timeout)))
  (when the-mp3
    (incf current-location)
    (incf x-rot))
  (q+:repaint fft-drawer))

(define-override (fft-drawer initialize-G-L) ()
  (gl:clear-color 0 0 0 1)
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test)
  )  

(define-override (fft-drawer resize-g-l) (width height)
  ;; (declare (ignore ev))
  (format t "version: ~a~%" (gl:get-string :version))
  (format t "~a x ~a ~%" width height)
  
  (gl:viewport 0 0 width height)

  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width -300 300 -1.0 1.0)
  ;; (glu:perspective 50 (/ height width) 1.0 5000.0)
  ;; (glu:look-at 200 200 200 
  ;;              0 0 0
  ;;              0 1 0)

  )

(define-override (fft-drawer paint-g-l paint) ()
  "Handle paint events."
  ;; (declare (ignore ev))
  (with-finalizing 
      ;; Create a painter object to draw on
      ((painter (q+:make-qpainter fft-drawer)))

    (gl:matrix-mode :modelview)
    (gl:load-identity)

    (gl:clear :color-buffer :depth-buffer)
    ;; Clear the background
    (when (and the-mp3 (< current-location total-frames))

      (let* ((height (q+:height fft-drawer))
             (width (q+:width fft-drawer))

             (fft-window-size (* 4096 1))
             (win-center (ceiling (max 0 
                                       (- (* 44100 (interpolate 0.0 song-duration current-location total-frames))
                                          (round (/ fft-window-size 2))))))

             (left-fft-data (bordeaux-fft:windowed-fft (mp3-file-left-channel the-mp3) win-center fft-window-size))
             (right-fft-data (bordeaux-fft:windowed-fft (mp3-file-right-channel the-mp3) win-center fft-window-size)))
        
          ;; Actual drawing goes here.  In this case, just a line.
          (gl:push-matrix)
          ;; ;; (gl:translate 0 (/ height 2.0) 0)
          ;; (gl:rotate 45 1 0 0)
          ;; (gl:rotate 45 0 0 1)
          ;; (gl:rotate x-rot 0 1 0)
          (gl:with-primitives :lines
            (loop
               for lft across left-fft-data
               for rgt across right-fft-data
               for idx below width
               do
                 (let ((left (truncate (- (abs lft))))
                       (right (truncate (abs rgt))))
                   
                   ;; (format t "left ~a right ~a idx ~a~%" left right idx)
                   (gl:color 1 0 0)
                   (gl:vertex idx left 0)
                   (gl:color 0 1 0)
                   (gl:vertex idx 0 0)
                   (gl:vertex idx 0 0)
                   (gl:color 1 0 0)
                   (gl:vertex idx right 0)

                   ;; (gl:color 0 1 0)
                   ;; (gl:vertex idx 0 left)
                   ;; (gl:color 1 0 0)
                   ;; (gl:vertex idx 0 0)

                   ;; (gl:vertex idx 0 0)
                   ;; (gl:color 0 1 0)
                   ;; (gl:vertex idx 0 right)
                   )))
          
          (gl:pop-matrix)))))


(define-subwidget (main-window viz-widget) (make-instance 'fft-drawer)
  "The fft-drawer itself.")

(define-slot (main-window open open-file) ()
  (let ((filename (q+:qfiledialog-get-open-file-name main-window "Select File"
                                                     (q+:qdesktopservices-storage-location 
                                                      (q+:qdesktopservices.music-location))
                                                     "*.mp3")))
    (if (and filename (> (length filename) 0))
        (let* ((new-mp3-file (read-mp3-file filename))
               (sduration (mp3-file-duration-in-seconds new-mp3-file))
               (tframes (ceiling (* sduration *fps*))))
          (setf (slot-value viz-widget 'current-location) 0)
          (setf (slot-value viz-widget 'x-rot) 0)
          (setf (slot-value viz-widget 'the-mp3) (copy-mp3-file new-mp3-file))
          (setf (slot-value viz-widget 'song-duration) sduration)
          (setf (slot-value viz-widget 'total-frames) tframes)
          (when current-stream (mixalot:mixer-remove-streamer mixer current-stream))
          (setf current-stream (mixalot-mp3:make-mp3-streamer filename))
          (mixalot:mixer-add-streamer mixer current-stream)))))

(define-initializer (main-window setup)
  "Set the window title and set the fft-controls to be the central widget."
  (setf (q+:window-title main-window) "Interactive FFT Explorer")
  (setf (q+:central-widget main-window) viz-widget))

(defun main ()
  "Create the main window."
  (with-main-window (window (make-instance 'main-window))))


