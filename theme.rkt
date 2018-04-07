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
        
        (define image (make-parameter (make-bitmap 1 1)))
        (define background-mixin
          (mixin (editor<%>) ()
            (super-new)
            (inherit invalidate-bitmap-cache)
            (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
              (when before?
                (send dc set-alpha 0.3)
                (send dc draw-bitmap-section (image) left top
                      left top
                      right bottom)
                (send dc set-alpha 1.0)
                )
              (super on-paint before? dc left top right bottom dx dy draw-caret)
              )
            (define/override (after-scroll-to)
              (invalidate-bitmap-cache 0.0 0.0 'display-end 'display-end)
              (super after-scroll-to)
              )
            
            
            ))
        (define editor-canvas-mixin
          (mixin ((class->interface drracket:unit:definitions-canvas%) canvas<%>) ()
            (super-new)
            (inherit get-dc get-editor)
            (define/override (on-paint)
              (super on-paint)
              (send (get-dc) set-alpha 0.3)
              (send (get-dc) draw-bitmap (image) 0 0)
              (send (get-dc) set-alpha 1.0)
              )
            ))

        (define frame-mixin
          (mixin (drracket:unit:frame<%>) ()
            (super-new)
            (inherit add-show-menu-items get-show-menu)
            (define set-background (new menu-item% [parent (get-show-menu)][label "Set Background"]
                                        [callback (lambda (a b) (put-preferences
                                                                 '(drracket-background)
                                                                 (list (path->string (get-file))))
                                                    (load-file!)
                                                    )]))
            ))
        (drracket:get/extend:extend-definitions-text background-mixin)
        #;(drracket:get/extend:extend-definitions-canvas editor-canvas-mixin)

        (drracket:get/extend:extend-unit-frame frame-mixin)
        (load-file!)
        ))
