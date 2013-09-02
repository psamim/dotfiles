" sexy_scroller.vim - Smooth animation of the cursor and the page whenever they move, with easing.
" By joeytwiddle, inspired by Terry Ma's smooth_scroll.vim, one of many Ctrl-U/Ctrl-D scrollers.
" Not to be confused with daylilyfield/sexyscroll.vim which lacks easing.
" I have not yet seen any other smooth-scrolling scripts that provide easing, operate on general movements, or use winrestview instead of keymappings/cursorhold.
"
" Usually when you scroll the buffer or motion to a different part of the
" document, Vim will jump there immediately.  But with SexyScroller, Vim will
" scroll to the new position smoothly.  As well as looking pretty, this
" provides you with visual feedback about the distance you have travelled.
"
" # Options
"
" Instead of specifying the scrolling *speed*, SexyScroller asks you to
" specify how *slow* you want scrolling to be.  You can store these options in
" your .vimrc once you are happy with them.
"
" Set the time it takes (in milliseconds) for the buffer to scroll one line or
" column.
"
"     :let g:SexyScroller_ScrollTime = 10
"
" Set the time it takes for the cursor to travel one line.
" Probably only visible if you have `:set cursorline`.  Set it to 0 to never
" animate the cursor.
"
"     :let g:SexyScroller_CursorTime = 5
"
" (I like to pretend the cursor is "lighter" than the buffer.)
"
" Set the maximum amount of time that longer scrolls can take:
"
"     :let g:SexyScroller_MaxTime = 500
"
" Choose the easing style (how scrolling accelerates and deccelerates):
"
"     :let g:SexyScroller_EasingStyle = 2
"
" where
"
"   - 1 = start fast, finish slowly            (like 2 but less so)
"   - 2 = start very fast, finish very slowly  (recommended, default)
"   - 3 = start slowly, get faster, end slowly (sexy)
"   - 4 = start very slowly, end very slowly   (like 3 but more so)
"   - ? = constant speed                       (dull)
"
" Interrupts the animation if you press a key.  Resumes the animation if they
" key you pressed causes further scrolling, otherwise just jumps directly to
" the destination.  Resuming animation looks best with EasingStyle 1 or 2.
"
"     :let g:SexyScroller_DetectPendingKeys = 1   /   0
"
" This command is provided to enable/disable the scrolling:
"
"     :SexyScrollerToggle
"
" For eye candy, try MaxTime=1200, EasingStyle=3 and increase ScrollTime as
" well.  This can help to visualise the distance travelled when moving through
" a document.
"
" Power users may prefer to lower MaxTime to 400, and set EasingStyle 1 or 0.
" This will make Vim feel more like normal (more responsive).

" # Issues
"
" - The script has trouble detecting a scroll event if the cursor did not move.  This is because we listen for CursorMoved events; Vim does not provide any WindowScrolled event!
"
"   If the script does fail to detect when a scroll occurs, it will eventually notice it later, when the cursor *does* move.  This will trigger an animation later than expected from a point which we already left some time ago.  This looks wrong!
"
"   Since this affects `<C-E>` and `<C-Y>`, we remap them to compensate.  However if you have mapped any alternative keybinds or commands which scroll the page without moving the cursor, these will not work.  A workaround is to append `<BS><Space>` to any such mappings, to move the cursor and move it back again!
"
"   Other keys similarly affected are the various `z` commands under |scroll-cursor|.  They are hard to map.
"
" - We cannot detect the scrolling through the document that occurs during a `/` search when 'incsearch' is enabled, again from a lack of events.  (This does not affect `n` and `N`, which work fine.)  TODO: We could temporarily disable animations when `/` or `?` are initiated and 'incsearch' is enabled (until the next CursorMoved or a CursorHold).  Alternatively, we could attempt to implement a fake `/` interface.
"
" - I have disabled smooth horizontal animation of the cursor because I can never see the cursor when it's moving, even with 'cursorcolumn' enabled!  Scrolling in this case is pointless: it just looks like Vim is briefly frozen!  In fact the cursor is also invisible during vertical scrolling, although 'cursorline' is visible while it is moving, if enabled.  CONSIDER TODO: A workaround for both axes might be to perform the requested movement as a sequence of feedkeys(), rather than calls to winrestview.  We would need to avoid re-triggering ourself on those CursorMoved events, with :noauto or with a flag.
"
" - PageUp, PageDown, Ctrl-U and Ctrl-D do not consistently trigger a getchar(), so DetectPendingKeys does not always work for them.  This may be system-dependent.  Simpler keystrokes like { and } never fail.  The result is that subsequent keypresses will not interrupt animation, but will start a second animation separately after the animation of the earlier keypresses.
"
" - Plugins which use :noauto (TagList for example) will not fire CursorMoved when they actually happen.  If we then focus the window later, this will lead to late detection and an out-of-date animation being performed.
"
" - Resizing the window may cause the topline/leftcol to change without firing a CursorMoved event, with the usual consequences.  This also happens when splitting a window.  Especially if you have scrolloff set!  TODO: Solution: Don't do any scrolling if we detect the size of the window has changed.
"
" - With 'cursorline' enabled, the cursor will animate after a mouse click, which does not look natural.  In this case, it should simply jump without any animation.  I cannot think of any way to fix this.
"
" - Animation does not work well with mouse scrolling.  I can't think of any way to work around this.  If you scroll with the mouse more than the keys, this plugin might start ao annoy you.  But it might be the thing that trains you to improve your Vimming.  :)
"
" - Our ability to scroll smoothly is limited by the presence of long wrapped lines at the top of the window. (For example if line 340 is long, wrapping to 6 screen lines, then since we cannot set 'topline' to "partway through line 340", the displayed text is forced to jump 6 lines when we set 'topline' to 341.)
"
" - Folded lines affect the effort/time-taken calculations.  So it takes MaxTime to scroll 1000 lines out of view, or move the cursor over them, even if those 1000 lines have been folded down and appear visually as one line!  TODO: To fix this we could check winline() throughout instead of winrestview()["lnum"].  NO that is a very poor check, because it is possible to jump 100 non-folded lines without the winline() changing.  Any suggestions how to fix this are welcome!
"
" CONSIDER TODO: Make a list of exclude keys, and map them so that they set w:SexyScroller_Ignore_Next_Movement.  For example this could apply to `/` and `?` with 'hlsearch' enabled, and maybe also to `d`.
"
" CONSIDER TODO: We could optionally enable cursorline whilst scrolling.  (Reproducing the functionality of highlight_line_after_jump.vim)
"
" TODO: We should politely store and restore lazyredraw if we are going to continue to clobber it.



if !has("float")
  echo "smooth_scroller requires the +float feature, which is missing"
  finish
endif



" == Options == "

if !exists("g:SexyScroller_Enabled")
  let g:SexyScroller_Enabled = 1
endif

" We can only really see the cursor moving if 'cursorline' is enabled
if !exists("g:SexyScroller_CursorTime")
  let g:SexyScroller_CursorTime = ( &cursorline || exists("g:hiline") && g:hiline ? 5 : 0 )
endif

" By default, scrolling the buffer is slower then moving the cursor, because a page of text is "heavier" than the cursor.  :)
if !exists("g:SexyScroller_ScrollTime")
  let g:SexyScroller_ScrollTime = 10
endif

if !exists("g:SexyScroller_MaxTime")
  let g:SexyScroller_MaxTime = 500
endif

if !exists("g:SexyScroller_EasingStyle")
  let g:SexyScroller_EasingStyle = 2
endif

if !exists("g:SexyScroller_DetectPendingKeys")
  let g:SexyScroller_DetectPendingKeys = 1
endif

if !exists("g:SexyScroller_Debug")
  let g:SexyScroller_Debug = 0
endif

if !exists("g:SexyScroller_DebugInterruption")
  let g:SexyScroller_DebugInterruption = 0
endif



" == Setup == "

command! SexyScrollerToggle call s:ToggleEnabled()

augroup Smooth_Scroller
  autocmd!
  autocmd CursorMoved * call s:CheckForChange(1)
  autocmd CursorMovedI * call s:CheckForChange(1)
  autocmd InsertLeave * call s:CheckForChange(0)
  " Unfortunately we would like to fire on other occasions too, e.g.
  " BufferScrolled, but Vim does not offer enough events for us to hook to!
augroup END

" |CTRL-E| and |CTRL-Y| scroll the window, but do not fire any events for us to detect.
" If the user has not made a custom mapping for them, we will map them to fix this.
if maparg("<C-E>", 'n') == ''
  nnoremap <silent> <C-E> <C-E>:call <SID>CheckForChange(1)<CR>
endif
if maparg("<C-Y>", 'n') == ''
  nnoremap <silent> <C-Y> <C-Y>:call <SID>CheckForChange(1)<CR>
endif



" == Functions == "

function! s:CheckForChange(actIfChange)
  let w:newPosition = winsaveview()
  let w:newBuffer = bufname('%')
  if a:actIfChange && g:SexyScroller_Enabled
        \ && exists("w:oldPosition")
        \ && exists("w:oldBuffer") && w:newBuffer==w:oldBuffer "&& mode()=='n'
    if s:differ("topline",3) || s:differ("leftcol",3) || s:differ("lnum",2) || s:differ("col",2)
        \ || exists("w:interruptedAnimationAt")
      if s:smooth_scroll(w:oldPosition, w:newPosition)
        return
      endif
    endif
  endif
  let w:oldPosition = w:newPosition
  let w:oldBuffer = w:newBuffer
endfunction

function! s:differ(str,amount)
  return abs( w:newPosition[a:str] - w:oldPosition[a:str] ) > a:amount
endfunction

function! s:smooth_scroll(start, end)

  let pi = acos(-1)

  "if g:SexyScroller_Debug
    "echo "Going from ".a:start["topline"]." to ".a:end["topline"]." with lnum from ".a:start["lnum"]." to ".a:end["lnum"]
    "echo "Target offset: ".(a:end["lnum"] - a:end["topline"])
  "endif

  let numLinesToTravel = abs( a:end["lnum"] - a:start["lnum"] )
  let numLinesToScroll = abs( a:end["topline"] - a:start["topline"] )
  let numColumnsToTravel = 0   " abs( a:end["col"] - a:start["col"] )   " No point animating cursor movement because I can't see the cursor during animation!
  let numColumnsToScroll = abs( a:end["leftcol"] - a:start["leftcol"] )
  let timeForCursorMove = g:SexyScroller_CursorTime * s:hypot(numLinesToTravel, numColumnsToTravel)
  let timeForScroll = g:SexyScroller_ScrollTime * s:hypot(numLinesToScroll, numColumnsToScroll)
  let totalTime = max([timeForCursorMove,timeForScroll])
  "let totalTime = timeForCursorMove + timeForScroll

  "if g:SexyScroller_Debug
    "echo "totalTime=".totalTime." cursor=".timeForCursorMove." (".numLinesToTravel.",".numColumnsToTravel.") scroll=".timeForScroll." (".numLinesToScroll.",".numColumnsToScroll.")"
  "endif

  let totalTime = 1.0 * min([g:SexyScroller_MaxTime,max([0,totalTime])])

  if totalTime < 1
    return
  endif

  let startTime = reltime()
  let current = copy(a:end)

  " Because arguments are immutable
  let start = a:start

  " If we have *just* interrupted a previous animation, then continue from where we left off.
  if exists("w:interruptedAnimationAt")
    let timeSinceInterruption = s:get_ms_since(w:interruptedAnimationAt)
    if g:SexyScroller_DebugInterruption
      echo "Checking interrupted animation, timeSince=".float2nr(timeSinceInterruption)." remaining=".float2nr(w:interruptedAnimationTimeRemaining)
    endif
    if timeSinceInterruption < 50
      let start = w:interruptedAnimationFrom
      if g:SexyScroller_DebugInterruption
        echo "Continuing interrupted animation with ".float2nr(w:interruptedAnimationTimeRemaining)." remaining, from ".start["topline"]
      endif
      " Secondary keystrokes should not make the animation finish sooner than it would have!
      if totalTime < w:interruptedAnimationTimeRemaining
        let totalTime = w:interruptedAnimationTimeRemaining
      endif
      " We could add the times together.  Not sure how I feel about this.
      "let totalTime = 1.0 * min([g:SexyScroller_MaxTime,float2nr(totalTime + w:interruptedAnimationTimeRemaining)])
    endif
    unlet w:interruptedAnimationAt
  endif

  " Since this function can be called if w:interruptedAnimationAt is set, it may sometimes be called unneccessarily, when we are already right next to the destination!  (Without checking, this would cause motion to slow down if I am holding a direction with a very fast keyboard repeat set.  It needs a long wrapped line or some folded lines in order to trigger it, after which interruptedAnimationAt keeps firing.)
  if numLinesToTravel<2 && numLinesToScroll<2 && numColumnsToTravel<2 && numColumnsToScroll<2
    return
  endif

  if g:SexyScroller_Debug
    echo "Travelling ".numLinesToTravel."/".numLinesToScroll." over ".float2nr(totalTime)."ms"
  endif

  while 1

    let elapsed = s:get_ms_since(startTime) + 8
    " +8 renders the position we should be in half way through the sleep 15m below.
    let thruTime = elapsed * 1.0 / totalTime
    if elapsed >= totalTime
      let thruTime = 1.0
    endif
    if elapsed >= totalTime
      break
    endif

    " Easing
    if g:SexyScroller_EasingStyle == 1
      let thru = cos( 0.5 * pi * (-1.0 + thruTime) )         " fast->slow
    elseif g:SexyScroller_EasingStyle == 2
      let c    = cos( 0.5 * pi * (-1.0 + thruTime) )
      let thru = sqrt(sqrt(c))                               " very fast -> very slow
    elseif g:SexyScroller_EasingStyle == 3
      let thru = 0.5 + 0.5 * cos( pi * (-1.0 + thruTime) )   " slow -> fast -> slow
    elseif g:SexyScroller_EasingStyle == 4
      let cpre = cos( pi * (-1.0 + thruTime) )
      let thru = 0.5 + 0.5 * sqrt(sqrt(abs(cpre))) * ( cpre > 0 ? +1 : -1 )    " very slow -> very fast -> very slow
    else
      let thru = thruTime
    endif

    let notThru = 1.0 - thru
    let current["topline"] = float2nr( notThru*start["topline"] + thru*a:end["topline"] + 0.5 )
    let current["leftcol"] = float2nr( notThru*start["leftcol"] + thru*a:end["leftcol"] + 0.5 )
    let current["lnum"] = float2nr( notThru*start["lnum"] + thru*a:end["lnum"] + 0.5 )
    let current["col"] = float2nr( notThru*start["col"] + thru*a:end["col"] + 0.5 )
    "echo "thruTime=".printf('%g',thruTime)." thru=".printf('%g',thru)." notThru=".printf('%g',notThru)." topline=".current["topline"]." leftcol=".current["leftcol"]." lnum=".current["lnum"]." col=".current["col"]

    call winrestview(current)
    redraw

    exec "sleep 15m"

    " Break out of the current animation if the user presses a new key.
    " Set some vars so that we can resume this animation from where it was interrupted, if the pending keys trigger further motion.
    " If they don't the animation simply jumps to the destination.
    if g:SexyScroller_DetectPendingKeys && getchar(1)
      let w:oldPosition = a:end
      let w:interruptedAnimationAt = reltime()
      let w:interruptedAnimationFrom = current
      let w:interruptedAnimationTimeRemaining = totalTime * (1.0 - thruTime)
      if g:SexyScroller_DebugInterruption
        echo "Pending keys detected at ".reltimestr(reltime())." remaining=".float2nr(w:interruptedAnimationTimeRemaining)
      endif
      " We must now jump to a:end, to be in the right place to process the next keypress, regardless whether it is a movement or edit command.
      " If we do end up resuming this animation, this winrestview will cause flicker, unless we set lazyredraw to prevent it.
      set lazyredraw
      call winrestview(a:end)
      return 0
      " Old approach:
      "let w:oldPosition = current
      "return 1
    endif

  endwhile

  call winrestview(a:end)

  return 0

endfunction

function! s:get_ms_since(time)   " Ta Ter
  let cost = split(reltimestr(reltime(a:time)), '\.')
  return str2nr(cost[0])*1000 + str2nr(cost[1])/1000.0
endfunction

function! s:hypot(x, y)
  "return max([a:x,a:y])
  return float2nr( sqrt(a:x*a:x*1.0 + a:y*a:y*1.0) )
endfunction

function! s:ToggleEnabled()
  let g:SexyScroller_Enabled = !g:SexyScroller_Enabled
  echo "Sexy Scroller is " . ( g:SexyScroller_Enabled ? "en" : "dis" ) . "abled"
endfunction




" vim: wrap textwidth=0 wrapmargin=0
