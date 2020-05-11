#|

(ql:quickload :sdl2)
(ql:quickload :sdl2-mixer)
(ql:quickload :sdl2)

|#

(defpackage #:oskar-color-game
  (:use :common-lisp)
  (:export :main))

(in-package :oskar-color-game)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-window (,window
                      :title "SDL2 Tutorial"
                      :w *screen-width*
                      :h *screen-height*
                      :flags '(:shown))
     (let ((,surface (sdl2:get-window-surface ,window)))
       ,@body)))

(defun load-images ()
  (list :default (sdl2:load-bmp "press.bmp")
        :up (sdl2:load-bmp "up.bmp")
        :down (sdl2:load-bmp "down.bmp")
        :left (sdl2:load-bmp "left.bmp")
        :right (sdl2:load-bmp "right.bmp")))

(defun load-music ()
  (list :good (sdl2-mixer:load-wav  #p"sample.ogg")))

(defmacro with-mixer-init (() &body body)
  `(progn
     (sdl2-mixer:init :ogg)
     (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
     (sdl2-mixer:allocate-channels 1)
     (unwind-protect
         (progn
           ,@body)
      (progn
        (sdl2-mixer:halt-music)
        (sdl2-mixer:close-audio)
        ;(sdl2-mixer:free-music music)
        (sdl2-mixer:quit)))))

(defun play-music (m)
  (sdl2-mixer:play-channel 0 m 0))

(defun main()
  (sdl2:with-init (:everything)
    (with-mixer-init ()
      (with-window-surface (window screen-surface)
        (let* ((images (load-images))
               (image (getf images :default))
               (musics (load-music)))
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:keydown (:keysym keysym)
                      (case (sdl2:scancode keysym)
                        (:scancode-space (play-music (getf musics :good)))
                        (:scancode-up (setf image (getf images :up)))
                        (:scancode-down (setf image (getf images :down)))
                        (:scancode-left (setf image (getf images :left)))
                        (:scancode-right (setf image (getf images :right)))
                        (t (setf image (getf images :default)))))
            (:idle ()
                   (sdl2:blit-surface image nil screen-surface nil)
                   (sdl2:update-window window)
                   (sdl2:delay 100))))))))    ;reduce cpu usage



(main)

(sdl2-mixer:)
