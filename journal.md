## RECORDING ON 11-11-22

NEXT STEPS:
* move in other dir.


## ARCHIVED ON 11-11-22

NEXT STEPS:
* [x] model game data
* [x] Board -> Dict Pos Int
* [x] initial board
* [x] move board
* [x] generate new elements unless game over
* [x] use translate instead of margin
* [x] animation
* [x] spawn random cells
* [x] view all transitions one after another
* [x] fix animation not being applied to new cells,
  merged or otherwise
    * probable cause, virtual dom and elm-css.
    * when a dom node gets reused animation doesn't get
      applied, if the animation-name for the old virtual node and new
      virtual node are same.
    * The new content and grid area props are just patched over.
    * Causing appropriate change in appearance,
      but not in animation-name prop.
* setup neovim
* enable vim emulation

Problem-Solving:
How to model 3 states of board?
* board only contains val
* in new phase we assume all entries are new
* we need to be able to distinguish, between board static val
  and newly added val's.
* perhaps we could store positions of newly added random val.

Can we address state changes for animation by using extra data?
* Board continues to hold only val.
* the acc collects moved/merged/static entries.
* which then can be used for animation.
* Can this be done.
* how can we ensure there is no impedance mismatch.
* initial board can be rendered as fadeIn.
* moved & merged can be rendered, from static, merged & moved
  entries. Same as now.
* additionally, let transitions store additional info it requires
  rather than clobbering board with it.
* sounds good.
  How to implement animation
* store state of each cell in board when move is performed.
* have animation states
* MoveTransitions
* New
* MoveAndMerge
* Static
* start with new.
* new phase goes to static.
* on slide, goto move&merge.
* on end of move and merge go to new.
* rendering val
* New: -> render new elements with opacity 0
    * transition: to opacity 1.
    * create anim fadeIn
    * apply to new elements.
    * show merged and moved elements as static.

    * MoveAndMerge: ->
        * fadeIn new merged elements.
        * show moved as static.
        * ignore new elements.

    * Static: show all elements as static. ignore old merged.
    * How about modifying the algorithm to give back additional
      data and keep the core board simple?
