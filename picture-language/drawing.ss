(module drawing scheme/gui
  (require "vectors.ss" "frames.ss")
  (provide WIDTH HEIGHT
           init-dc save-dc
           draw-line draw-and-save
           whole-dc-frame)

  (define WIDTH 750)
  (define HEIGHT 750)

  (define PEN-COLOR "BLACK")
  (define PEN-WIDTH 1)
  (define PEN-TYPE 'solid)

  (define PIC-FORMAT 'png)
  (define PIC-EXTENSION ".png")

  (define DC-SMOOTHING-MODE 'aligned)

  (define global-bitmap (make-object bitmap% WIDTH HEIGHT))
  (define global-dc (make-object bitmap-dc% global-bitmap))
  (define global-pen (make-object pen% PEN-COLOR PEN-WIDTH PEN-TYPE))

  (define (init-dc)
    (send global-dc set-smoothing DC-SMOOTHING-MODE)
    (send global-dc set-pen global-pen)
    (send global-dc clear))

  (define (save-dc filename)
    (let* ((bitmap (send global-dc get-bitmap))
           (ext-length (string-length PIC-EXTENSION))
           (filename-length (string-length filename))
           (correct-ext? (string-ci=? PIC-EXTENSION
                                      (substring filename (- filename-length ext-length))))
           (filename (if correct-ext?
                         filename
                         (string-append filename PIC-EXTENSION))))
      (send bitmap save-file filename 'png)))

  (define (draw-line start end)
    (send global-dc draw-line
          (xcor-vect start)
          (- HEIGHT (ycor-vect start))
          (xcor-vect end)
          (- HEIGHT (ycor-vect end))))

  (define whole-dc-frame (make-frame (make-vect 0 0)
                                     (make-vect WIDTH 0)
                                     (make-vect 0 HEIGHT)))

  (define (draw-and-save painter filename)
    (init-dc)
    (painter whole-dc-frame)
    (save-dc filename)))
