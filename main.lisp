#|

(ql:quickload :sdl2)

|#

(defpackage #:oskar-color-game
  (:use :common-lisp)
  (:export :main))

(in-package :oskar-color-game)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (let ((,surface (sdl2:get-window-surface ,window)))
         ,@body))))

(defun load-media ()
  (list :default (sdl2:load-bmp "press.bmp")
        :up (sdl2:load-bmp "up.bmp")
        :down (sdl2:load-bmp "down.bmp")
        :left (sdl2:load-bmp "left.bmp")
        :right (sdl2:load-bmp "right.bmp")))

(defun main()
  (with-window-surface (window screen-surface)
    (let* ((images (load-media))
           (image (getf images :default)))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown (:keysym keysym)
                  (case (sdl2:scancode keysym)
                    (:scancode-up (setf image (getf images :up)))
                    (:scancode-down (setf image (getf images :down)))
                    (:scancode-left (setf image (getf images :left)))
                    (:scancode-right (setf image (getf images :right)))
                    (t (setf image (getf images :default)))))
        (:idle ()
               (sdl2:blit-surface image nil screen-surface nil)
               (sdl2:update-window window)
               (sdl2:delay 100))))))    ;reduce cpu usage

(main)
