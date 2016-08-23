;;;; qt-3d-fft.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:qt-3d-fft
  :description "Interactive FFT explorer."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:qtools
               #:qtgui
               #:qtcore
               #:anim-utils
               #:mixalot-mp3
               #:qtopengl
               #:cl-opengl
               #:cl-glu
               #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "qt-3d-fft")))
