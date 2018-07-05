#lang racket
;;DrRacket Background
;;

(require racket/gui
         drracket/tool
         racket/unit
         racket/class
         racket/draw)
(provide tool@)
(define tool@
  (unit (import drracket:tool^)
        (export drracket:tool-exports^)
        (define phase1 void)
        (define phase2 void)
        
        (define (load-file!)
          (define f (get-preference 'drracket-background))
          (when f (image (read-bitmap f))))
        
        (define (calc-start-point editor)
          (if (send editor get-canvas)
              (let-values ([(cwidth cheight) (send (send editor get-canvas) get-client-size)])
                (let*  [(h (get-preference 'drracket-background-alignmentH))
                        (v (get-preference 'drracket-background-alignmentV))
                        (descent (* 2 (send editor get-descent)))
                        (cwidth (- cwidth descent))
                        (cheight (- cheight descent))
                        (width (send (image) get-width))
                        (height (send (image) get-height))
                        (left-start (if (eq? h 'right) (- cwidth width) 0))
                        (right-start (if (eq? v 'bottom) (- cheight height) 0))]
                  (values left-start right-start)))
              (values 0 0)
              ))
                                 
        
        (define image (make-parameter (make-bitmap 1 1)))


          
        (define background-mixin
          (mixin (editor<%>) ()
            (super-new)
            (inherit invalidate-bitmap-cache get-canvas)
            (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
              (when before?
                (define-values (sx sy) (calc-start-point this))
                (send dc set-alpha 0.3)
                (send dc draw-bitmap-section (image) left top
                      (- left sx) (- top sy) 
                      (- right left) (- bottom top))
                (send dc set-alpha 1.0)
                )
              (super on-paint before? dc left top right bottom dx dy draw-caret)
              )
            (define/override (after-scroll-to)
              (invalidate-bitmap-cache 0.0 0.0 'display-end 'display-end)
              (super after-scroll-to)
              )
            
            
            ))


        (define frame-mixin
          (mixin (drracket:unit:frame<%>) ()
            (super-new)
            (inherit add-show-menu-items get-show-menu) 
            (define set-background (new menu-item% [parent (get-show-menu)][label "Set Background"]
                                        [callback (lambda (a b) (define f (get-file))
                                                    (when f (put-preferences
                                                             '(drracket-background)
                                                             (list (path->string f)))
                                                      (load-file!))
                                                    )]))

            (define set-alignment (new menu-item% [parent (get-show-menu)][label "Set Background Alignment"]
                                       [callback (lambda (a b) 
                                                   (send set-frame show #t)
                                                   )])) 
            (define set-frame (new frame% [label "Set Background Alignment"]
                                   [width 300] [height 150]))
            (define setH (new choice% [label "Honrizontal Alignment"]
                              [choices '("left" "right")]
                              [parent set-frame]
                              [selection (if (eq? (get-preference 'drracket-background-alignmentH) 'right)
                                             1 0)]
                              [callback (lambda (c e) (put-preferences '(drracket-background-alignmentH)
                                                                       (list (if (eq? (send c get-selection) 1)
                                                                                 'right
                                                                                 'left))))]
                              ))
            (define setV (new choice% [label "Vertical Alignment"]
                              [choices '("top" "bottom")]
                              [parent set-frame]
                              [selection (if (eq? (get-preference 'drracket-background-alignmentV) 'bottom)
                                             1 0)]
                              [callback (lambda (c e) (put-preferences '(drracket-background-alignmentV)
                                                                       (list (if (eq? (send c get-selection) 1)
                                                                                 'bottom
                                                                                 'top))))]
                              ))
            
            ))
          
        (drracket:get/extend:extend-definitions-text background-mixin)
        (drracket:get/extend:extend-unit-frame frame-mixin)
        (load-file!)
        ))
