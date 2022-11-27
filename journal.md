## RECORDING ON 27-11-22

CURRENT STATE:
* UI work pending
* Using list of tiles without id, and resetting animation on update.
    * keyed node with update counter for tiles animation
    * keyed node with score total for score delta animation
* Using elm-css for animation, no manual animation frame.

NEXT STEPS:
* [ ] game over colors
* [ ] game over ui
* [ ] tile colors
* [ ] score and new game UI
* [ ] header
* [ ] footer





THINKING

Should we use tile id?
PROS: 
* all dom nodes need not be recreated from scratch, perhaps it will help in future.
* so that we can get rid of global update counter.
* can switch to move transition instead of animation.

CONS:
* Complexity.
* Will have to pass id creation code around.
* Will have to use a timer for removing exit nodes (score delta and tiles).
* Switching to transitions, will cause unnecessary artifacts when rapidly making moves.

CONCLUSION: NO UPSIDE.


























## RECORDING ON 15-11-22, ARCHIVED ON 27-11-22

CURRENT STATE:
* We have 2 implementations with and without animations.

NEXT STEPS:
* [x] Merge implementations. (with and without animation)
* [x] bug: new tiles are generated without move/merge
* [x] game over
* [x] generate 2 new tiles at start and one afterwards.
* [x] experiment to find when can we remove exit node without messing up dom animations.
* [ ] ui styles



Problem-Solving:

How to determine if slide and merge operation resulted in any change?
* We can't compare old and new board, since old board contains old anim state
* and new board contains stayed anim state.
* This anim change will happen even if there is no actual tile movement.
* Also, the two grids we are using are not type compatible.
* But we could use same type for both grid to solve this problem
* and also simplify code.
* Wow!!
* Nope!!
* Input cannot contain merged type, it is an impossible state.
* so how do we handle it, while folding a row?
* We could convert input `idval` grid to Unmerged and then compare :)
* let's try that.

What will be initial state?
* {tiles: Dict Id Tile, grid: Grid Id}
* what is id? val? idVal? why should they be separate?
* 
How to merge implementations?
* List Tile `<->` Grid4x4 Int
```
type alias Tile =
    { pos : Int2
    , id : String
    , val : Int
    , anim : Anim
    }
```
* position & anim updates for existing tiles,
  as well as new tiles can be derived from grid.
* input grid cell : {id,val}
* intermediate cell: `Merged id id val | Updated id val`.
* update exit tiles from merged  
  and stayed tiles from updated.
* at this point no tiles should be in state of:
  InitialEnter, MergedEnter, NewEnter
* i.e. all enter `anims` should be changed to `stayed`.
* Before starting board updates.
* there are 3 primary types enter/exit/stayed.
* And a new board will be composed of either an `enter` or a `stayed` tile.
* So with dict of tiles and grid4x4 we can solve this problem.
* although all the states that are modeled are not ideal.
* we can get back to modeling after the implementation is complete.

























## RECORDING ON 11-11-22, ARCHIVED ON 15-11-22

NEXT STEPS:
* [x] move in other dirs.
* [x] arrow key handling
* [x] figure out how keyed v-dom nodes behave
  when their dom order is shuffled.
    * what happens to their transforms/animations.
    * Answer: 
    * key is needed so that anim/trans don't carry over to other nodes
    * item order needs to be maintained, or else new nodes might get
    * created, even for keyed items. if they move too much in the list.
    * For example, if the list is reversed.
    * Item deletions will also cause problems, when combined with shuffling of
    * items.
    * when new nodes are mapped to existing items
    * transitions end abruptly, and animations are reapplied.
    * and when nodes get reused because of lack of key
    * unforeseen transitions are carried over. 
    * And animations don't start if they were shared with previous node.
    * Complex Shit.
* [x] React: figure out how keyed v-dom nodes behave
      when their dom order is shuffled
      * same issues!!!
* [ ]create 2 implementations 
  * [x] one only with core logic
  * [x] another for transition.
* [ ] get rid of `Process.sleep` for triggering board updates.
  preferably maintain entire board state after a move is performed.


Problem-Solving:

Modeling animations.
* let's focus on animations, without worrying about core game logic.
* two tiles will appear.
* both will slide and merge into new tile.
* two more will appear.
* let slide up be a single scheduled event.
* Wow! done

How to model transitions and board tile logic together?
* Board can be list of Tiles
* type Tile/State = TNew Pos Val | TMoved Pos Val | TExit Pos Val | TMerged Pos Val
* above representation will make it very easy to render a tile.
* type alias Tile = {pos:Pos,val:Val,state:State}
* Dict Key Tile
* initially there will be new, let val contain key.
* Slide: 
* Reset
* List / Dict Tile 
      -> Mat4 (Input (Pos,Val) | None) 
      -> Mat4 (Stayed (Pos, Val) | Merged (Pos,Val) (Pos, Val) | None)
      -> List / Dict Tile
* this seems feasible

How to implement transitions on top of Vector4Board?
* what do we need inorder to implement transitions?
  * if we are going to use css transitions, then we need a unique key
    associated with each tile.
  * merge and slide logic needs to return states of each tile.
* Can we implement merge and slide without requiring a key?
  very likely
* BLOCKER: We haven't implemented adding random entries.
* Do we really need board to add random entries?
* perhaps we can implement it outside board.
* What about separating core logic and animation?
* How much can we actually separate?
* Well this line of thought is moot. 
* For now lets implement without thinking about reuse.
























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

