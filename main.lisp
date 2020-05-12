
(ql:quickload :sdl2)
(ql:quickload :sdl2-mixer)
(ql:quickload :sdl2)


(defpackage #:oskar-color-game
  (:use :common-lisp)
  (:export :main))

(in-package :oskar-color-game)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-window (,window
                      :title "Color Game for Oskar"
                      :w *screen-width*
                      :h *screen-height*
                      :flags '(:shown))
     (let ((,surface (sdl2:get-window-surface ,window)))
       ,@body)))

(defun load-images ()
  (list
        :sun (sdl2:load-bmp "sun.bmp")
        :sea (sdl2:load-bmp "sea.bmp")
        :forrest (sdl2:load-bmp "forrest.bmp")
        :apple (sdl2:load-bmp "apple.bmp")))

(defvar *images*)

(defun load-music ()
  (list :good (sdl2-mixer:load-wav  #p"sample.ogg")))

(defvar *musics*)

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

(defvar *script*)

(setf *script*
  '((:yellow :sun)
    (:blue :sea)
    (:green :forrest)
    (:red :apple)))
    

(defun random-from-script ()
  (elt *script* (random (length *script*))))

(defvar *current*)

(defun eval-guess (color)
  (when (equal (car *current*) color)
    (play-music (getf *musics* :good))
    (setf *current* (random-from-script))))

(defun main()
  (sdl2:with-init (:everything)
    (with-mixer-init ()
      (with-window-surface (window screen-surface)
        (let* ((*images* (load-images))
               (*current* (random-from-script))
               (*musics* (load-music)))
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:keydown (:keysym keysym)
                      (case (sdl2:scancode keysym)
                        (:scancode-8 (eval-guess :yellow))
                        (:scancode-kp-8 (eval-guess :yellow))
                        (:scancode-2 (eval-guess :blue))
                        (:scancode-kp-2 (eval-guess :blue))
                        (:scancode-6 (eval-guess :green))
                        (:scancode-kp-6 (eval-guess :green))
                        (:scancode-4 (eval-guess :red))
                        (:scancode-kp-4 (eval-guess :red))
                        (t (format t "pressed ~a~%" (sdl2:scancode keysym)))))
            (:idle ()
                   (sdl2:blit-surface (getf *images* (second *current*)) nil
                                      screen-surface nil)
                   (sdl2:update-window window)
                   (sdl2:delay 100))))))))    ;reduce cpu usage



(main)
  
