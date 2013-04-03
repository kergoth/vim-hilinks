" hilinks.vim: the source herein generates a trace of what
"              highlighting groups link to what highlighting groups
"
"  Author:		Charles E. Campbell <NdrOchipS@PcampbellAfamily.Mbiz>
"  Date:		Feb 28, 2013
"  Version:		4k	ASTRO-ONLY
"
"  NOTE:        This script requires Vim 6.0 (or later)
"               Works best with Vim 7.1 with patch#215
"
"  Usage: {{{1
"
"  \hlt   : will reveal a linked list of highlighting from the top-level down
"           to the bottom level.
"           You may redefine the leading character using "let mapleader= ..."
"           in your <.vimrc>.
"
"  History: {{{1
"   3 04/07/05 :	* cpo&vim supported
"   2 07/14/04 :	* register a is used as before but now its original contents are restored
"   				* bugfix: redraw taken before echo to fix message display
"   				* debugging code installed
"  	1 08/01/01 :	* the first release
" ---------------------------------------------------------------------
"  Load Once: {{{1
if exists("g:loaded_hilinks") || &cp
  finish
endif
let g:loaded_hilinks= "v4k"
if v:version < 700
 echohl WarningMsg
 echo "***warning*** this version of hilinks needs vim 7.0"
 echohl Normal
 finish
endif
let s:keepcpo= &cpo
set cpo&vim
"DechoRemOn

" ---------------------------------------------------------------------
"  Initialization: {{{1
let s:HLTmode       = 0

" ---------------------------------------------------------------------
" Public Interface: {{{1
if !hasmapto('<Plug>HiLinkTrace')
 map <script> <s-F10>		<Leader>hlt
 map <unique> <Leader>hlt	<Plug>HiLinkTrace
endif
map <script> <Plug>HiLinkTrace	:sil! call <SID>HiLinkTrace(0)<CR>
com! -bang	HLT					sil! call s:HiLinkTrace(<bang>0)
com!		HLTm				sil! call s:HiLinkTrace(1)

" ---------------------------------------------------------------------
"  Options: {{{1
if !exists("g:hilinks_fmtwidth")
 let g:hilinks_fmtwidth= 35
endif

" ---------------------------------------------------------------------
" HiLinkTrace: this function traces the highlighting group names {{{1
"             from transparent/top level through to the bottom
fun! <SID>HiLinkTrace(always)
"  call Dfunc("HiLinkTrace(always=".a:always.")")

  " save register a
  let keep_rega= @a

  " get highlighting linkages into register "a"
  redir @a
   silent! hi
  redir END
"  call Decho("reg-A now has :hi output")

  " initialize with top-level highlighting
  if a:always == 2
   let curline   = v:beval_lnum
   let curcol    = v:beval_col
"   call Decho("curline#".curline." curcol#".curcol." (used mouse location)")
  else
   let curline   = line(".")
   let curcol    = col(".")
"   call Decho("curline#".curline." curcol#".curcol." (used cursor location)")
  endif
  let firstlink = synIDattr(synID(curline,curcol,1),"name")
  let lastlink  = synIDattr(synIDtrans(synID(curline,curcol,1)),"name")
  let translink = synIDattr(synID(curline,curcol,0),"name")                
"  call Decho("firstlink<".firstlink."> lastlink<".lastlink."> translink<".translink.">")

  " if transparent link isn't the same as the top highlighting link,
  " then indicate it with a leading "T:"
  if firstlink != translink
   let hilink= "T:".translink."->".firstlink
"   call Decho("firstlink!=translink<".hilink.">")
  else
   let hilink= firstlink
"   call Decho("firstlink==translink<".hilink.">")
  endif

  " trace through the linkages
  if firstlink != lastlink
   let no_overflow= 0
   let curlink    = firstlink
"   call Decho("loop#".no_overflow.": hilink<".hilink.">")
   while curlink != lastlink && no_overflow < 10
   	let no_overflow = no_overflow + 1
   	let nxtlink     = substitute(@a,'^.*\<'.curlink.'\s\+xxx links to \(\a\+\).*$','\1','')
	if nxtlink =~ '\<start=\|\<cterm[fb]g=\|\<gui[fb]g='
	 let nxtlink= substitute(nxtlink,'^[ \t\n]*\(\S\+\)\s\+.*$','\1','')
   	 let hilink = hilink ."->". nxtlink
	 break
	endif
"    call Decho("loop#".no_overflow.": curlink<".curlink."> nxtlink<".nxtlink."> hilink<".hilink.">")
   	let hilink      = hilink ."->". nxtlink
   	let curlink     = nxtlink
   endwhile
"   call Decho("endloop: hilink<".hilink.">")
  endif

  " Use new synstack() function, available with 7.1 and patch#215
  if v:version > 701 || ( v:version == 701 && has("patch215"))
   let syntaxstack = ""
   let isfirst     = 1
   let idlist      = synstack(curline,curcol)
   if !empty(idlist)
    for id in idlist
     if isfirst
      let syntaxstack= syntaxstack." ".synIDattr(id,"name")
      let isfirst = 0
     else
      let syntaxstack= syntaxstack."->".synIDattr(id,"name")
     endif
    endfor
   endif
  endif

  " display hilink traces
  redraw
  let synid= hlID(lastlink)

  if !exists("syntaxstack")
   let retval= printf("HltTrace: %-".g:hilinks_fmtwidth."s fg<%s> bg<%s>",hilink,synIDattr(synid,"fg"),synIDattr(synid,"bg"))
  else
   let retval= printf("SynStack: %-".g:hilinks_fmtwidth."s  HltTrace: %-".g:hilinks_fmtwidth."s fg<%s> bg<%s>",syntaxstack,hilink,synIDattr(synid,"fg"),synIDattr(synid,"bg"))
  endif
  if a:always == 0 || a:always == 1
   echo retval
  endif

  " restore register a
  let @a= keep_rega

  " set up CursorMoved autocmd on bang
  if a:always == 1
   if !s:HLTmode
	" install a CursorMoved highlighting trace
"	call Decho("install CursorMoved HLT")
	let s:HLTmode= 1
	augroup HLTMODE
	 au!
	 au CursorMoved * call s:HiLinkTrace(0)
	augroup END
   else
	" remove the CursorMoved highlighting trace
"	call Decho("remove CursorMoved HLT")
	let s:HLTmode= 0
	augroup HLTMODE
	 au!
	augroup END
	augroup! HLTMODE
   endif

   if v:version >= 700 && has("balloon_eval") && !exists("g:hilinks_nobeval") && a:always == 1
	if !exists("s:initbeval")
"	 call Decho("saving beval, bexpr")
	 let s:initbeval= &beval
	 let s:initbexpr= &bexpr
	endif
    if s:HLTmode
	 " install a balloon eval for highlight tracing
"	 call Decho("setting beval")
     set beval
	 exe "let &bexpr= '".s:SID()."HiLinkTrace(2)'"
"	 call Decho("installing bexpr=".&bexpr)
	else
	 " restore balloon eval/expr to initial values
"	 call Decho("restoring beval, bexpr")
	 let &beval= s:initbeval
	 let &bexpr= s:initbexpr
	endif
   endif
  endif

  if a:always == 2
"   call Dret("HiLinkTrace <".hilink.">")
   return retval
  endif

"  call Dret("HiLinkTrace : hilink<".hilink.">")
endfun

" ---------------------------------------------------------------------
" s:SID: determine what <SID> normally maps to {{{1
function s:SID()
"  call Dfunc("s:SID()")
  let retval= '<SNR>'.matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$').'_'
"  call Dret("s:SID ".retval." : sfile<".expand('<sfile>'))
  return retval
endfun

" =====================================================================
"  Restore: {{{1
let &cpo= s:keepcpo
" vim: fdm=marker
