if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
inoremap <silent> <Plug>Tex_Completion :call Tex_Complete("default","text")
imap <silent> <Plug> <Nop>
inoremap <silent> <Plug>Tex_LeftRight =Tex_LeftRight()
inoremap <silent> <Plug>Tex_MathCal =Tex_MathCal()
inoremap <silent> <Plug>Tex_MathBF =Tex_MathBF()
inoremap <silent> <Plug>Tex_FastCommandChange :call Tex_ChangeCommand('no')
inoremap <silent> <Plug>Tex_FastCommandInsert =Tex_DoCommand('no')
inoremap <silent> <Plug>Tex_FastEnvironmentChange :call Tex_ChangeEnvironments()
inoremap <silent> <Plug>Tex_FastEnvironmentInsert =Tex_FastEnvironmentInsert("no")
imap <F1> <Plug>Tex_Help
imap <silent> <Plug>IMAP_JumpBack =IMAP_Jumpfunc('b', 0)
imap <silent> <Plug>IMAP_JumpForward =IMAP_Jumpfunc('', 0)
inoremap <silent> <expr> <Plug>AcpOnPopupPost acp#onPopupPost()
imap <silent> <F7> :exe ":ptag ".expand("<cword>")
imap <silent> <F6> :call GenTags()
imap <silent> <F4> :nohl
imap <silent> <F3> :silent make
imap <silent> <F2> :silent write
nnoremap  :cprev 
smap 	 i<BS>	
vmap <NL> <Plug>IMAP_JumpForward
nmap <NL> <Plug>IMAP_JumpForward
onoremap <NL> :bn 
noremap  :bp 
nnoremap  :cnext 
nmap i :cs find i ^=expand("<cfile>")$
nmap f :cs find f =expand("<cfile>")
nmap e :cs find e =expand("<cword>")
nmap t :cs find t =expand("<cword>")
nmap c :cs find c =expand("<cword>")
nmap d :cs find d =expand("<cword>")
nmap g :cs find g =expand("<cword>")
nmap s :cs find s =expand("<cword>")  
noremap   <PageDown>
vnoremap , :call ExecMap(',', 'v')
vnoremap ,pi :call VEnclose('', '', '\begin{picture}', '\end{picture}')
vnoremap ,mp :call VEnclose('', '', '\begin{minipage}', '\end{minipage}')
vnoremap <silent> ,lr :call VEnclose('\sbox{', '}', '\begin{lrbox}', '\end{lrbox}')
vnoremap ,fi :call VEnclose('', '', '\begin{figure}', '\end{figure}')
vnoremap ,pg :call VEnclose('\paragraph{', '}', '', '')
vnoremap ,s2 :call VEnclose('\subsubsection{', '}', '', '')
vnoremap ,ss :call VEnclose('\subsection{', '}', '', '')
vnoremap ,se :call VEnclose('\section{', '}', '', '')
vnoremap ,ch :call VEnclose('\chapter{', '}', '', '')
vnoremap ,pa :call VEnclose('\part{', '}', '', '')
vnoremap <silent> ,sl :call VEnclose('', '', '\begin{slide}', '\end{slide}')
vnoremap <silent> ,ov :call VEnclose('', '', '\begin{overlay}', '\end{overlay}')
vnoremap <silent> ,no :call VEnclose('', '', '\begin{note}', '\end{note}')
vnoremap <silent> ,tb :call VEnclose('', '', '\begin{thebibliography}', '\end{thebibliography}')
vnoremap <silent> ,ve :call VEnclose('', '', '\begin{verse}', '\end{verse}')
vnoremap <silent> ,vm :call VEnclose('\verb|', '|', '\begin{verbatim}', '\end{verbatim}')
vnoremap <silent> ,tp :call VEnclose('', '', '\begin{titlepage}', '\end{titlepage}')
vnoremap ,sp :call VEnclose('\subparagraph{', '}', '', '')
vnoremap <silent> ,qe :call VEnclose('', '', '\begin{quote}', '\end{quote}')
vnoremap <silent> ,qn :call VEnclose('', '', '\begin{quotation}', '\end{quotation}')
vnoremap <silent> ,fr :call VEnclose('{\raggedright ', '}', '\begin{flushright}', '\end{flushright}')
vnoremap <silent> ,fl :call VEnclose('', '', '\begin{flushleft}', '\end{flushleft}')
vnoremap <silent> ,fc :call VEnclose('', '', '\begin{filecontents}', '\end{filecontents}')
vnoremap <silent> ,do :call VEnclose('', '', '\begin{document}', '\end{document}')
vnoremap <silent> ,ce :call VEnclose('\centerline{', '}', '\begin{center}', '\end{center}')
vnoremap <silent> ,ap :call VEnclose('', '', '\begin{appendix}', '\end{appendix}')
vnoremap <silent> ,ab :call VEnclose('', '', '\begin{abstract}', '\end{abstract}')
vnoremap <silent> ,ma :call VEnclose('', '', '\begin{math}', '\end{math}')
vnoremap <silent> ,eq :call VEnclose('', '', '\begin{equation}', '\end{equation}')
vnoremap <silent> ,ea :call VEnclose('', '', '\begin{eqnarray}', '\end{eqnarray}')
vnoremap <silent> ,dm :call VEnclose('', '', '\begin{displaymath}', '\end{displaymath}')
vnoremap ,ar :call VEnclose('', '', '\begin{array}', '\end{array}')
vnoremap ,tr :call VEnclose('', '', '\begin{tabular}', '\end{tabular}')
vnoremap <silent> ,tg :call VEnclose('', '', '\begin{tabbing}', '\end{tabbing}')
vnoremap ,te :call VEnclose('', '', '\begin{table}', '\end{table}')
vnoremap <silent> ,tl :call VEnclose('', '', '\begin{trivlist}', '\end{trivlist}')
vnoremap <silent> ,ti :call VEnclose('', '', '\begin{theindex}', '\end{theindex}')
vnoremap <silent> ,it :call VEnclose('', '', '\begin{itemize}', '\end{itemize}')
vnoremap <silent> ,en :call VEnclose('', '', '\begin{enumerate}', '\end{enumerate}')
vnoremap ,de :call VEnclose('', '', '\begin{description}', '\end{description}')
vnoremap ,li :call VEnclose('', '', '\begin{list}', '\end{list}')
map ,F :set foldlevel=100
map ,f :set foldlevel=0
map ,e :e =expand("%:h")/
nmap K <Plug>ManPageView
vnoremap Q gq
nnoremap Q gqap
xmap S <Plug>VSurround
vmap [% [%m'gv``
map <silent> \w\t <Plug>VimwikiTabMakeDiaryNote
map <silent> \w\w <Plug>VimwikiMakeDiaryNote
map <silent> \ws <Plug>VimwikiUISelect
map <silent> \wt <Plug>VimwikiTabGoHome
map <silent> \ww <Plug>VimwikiGoHome
nmap <silent> \cv <Plug>VCSVimDiff
nmap <silent> \cu <Plug>VCSUpdate
nmap <silent> \cU <Plug>VCSUnlock
nmap <silent> \cs <Plug>VCSStatus
nmap <silent> \cr <Plug>VCSReview
nmap <silent> \cq <Plug>VCSRevert
nmap <silent> \cn <Plug>VCSAnnotate
nmap <silent> \cN <Plug>VCSSplitAnnotate
nmap <silent> \cl <Plug>VCSLog
nmap <silent> \cL <Plug>VCSLock
nmap <silent> \ci <Plug>VCSInfo
nmap <silent> \cg <Plug>VCSGotoOriginal
nmap <silent> \cG <Plug>VCSClearAndGotoOriginal
nmap <silent> \cd <Plug>VCSDiff
nmap <silent> \cD <Plug>VCSDelete
nmap <silent> \cc <Plug>VCSCommit
nmap <silent> \ca <Plug>VCSAdd
map \mbt <Plug>TMiniBufExplorer
map \mbu <Plug>UMiniBufExplorer
map \mbc <Plug>CMiniBufExplorer
map \mbe <Plug>MiniBufExplorer
nmap \caL <Plug>CalendarH
nmap \cal <Plug>CalendarV
map \o :call CopyCat() 
map \ft :cscope find t 
map \fs :cscope find s 
map \fi :cscope find i 
map \fg :cscope find g 
map \ff :cscope find f 
map \fe :cscope find e 
map \fd :cscope find d 
map \fc :cscope find c 
vmap ]% ]%m'gv``
vnoremap ` :call ExecMap('`', 'v')
vnoremap <silent> `em :call VEnclose('\emph{', '}', '{\em', '\/}')
vnoremap <silent> `it :call VEnclose('\textit{', '}', '{\itshape ', '}')
vnoremap <silent> `sc :call VEnclose('\textsc{', '}', '{\scshape ', '}')
vnoremap <silent> `sl :call VEnclose('\textsl{', '}', '{\slshape ', '}')
vnoremap <silent> `up :call VEnclose('\textup{', '}', '{\upshape ', '}')
vnoremap <silent> `rm :call VEnclose('\textrm{', '}', '{\rmfamily ', '}')
vnoremap <silent> `sf :call VEnclose('\textsf{', '}', '{\sffamily ', '}')
vnoremap <silent> `tt :call VEnclose('\texttt{', '}', '{\ttfamily ', '}')
vnoremap <silent> `md :call VEnclose('\textmd{', '}', '{\mdseries ', '}')
vnoremap <silent> `bf :call VEnclose('\textbf{', '}', '{\bfseries ', '}')
vnoremap <silent> `$ :call VEnclose('$', '$', '\[', '\]')
vnoremap <silent> `{ :call VEnclose('\left\{ ', ' \right\}', '\left\{', '\right\}')
vnoremap <silent> `[ :call VEnclose('\left[ ', ' \right]', '\left[', '\right]')
vnoremap <silent> `( :call VEnclose('\left( ', ' \right)', '\left(', '\right)')
vmap a% [%v]%
nmap cs <Plug>Csurround
nmap ds <Plug>Dsurround
nmap gx <Plug>NetrwBrowseX
xmap gS <Plug>VgSurround
map g :cs find s =expand("<cword>")
map g :cs find c =expand("<cword>")
map j gj
map k gk
xnoremap <silent> s :echoerr 'surround.vim: Visual mode s has been removed in favor of S'
nmap ySS <Plug>YSsurround
nmap ySs <Plug>YSsurround
nmap yss <Plug>Yssurround
nmap yS <Plug>YSurround
nmap ys <Plug>Ysurround
nnoremap <silent> <Plug>Tex_LeftRight :call Tex_PutLeftRight()
vnoremap <silent> <Plug>Tex_MathCal `>a}`<i\mathcal{
vnoremap <silent> <Plug>Tex_MathBF `>a}`<i\mathbf{
nnoremap <Plug>Tex_RefreshFolds :call MakeTexFolds(1)
nnoremap <silent> <Plug>Tex_FastCommandChange :call Tex_ChangeCommand('no')
vnoremap <silent> <Plug>Tex_FastCommandInsert :call Tex_DoCommand('yes')
nnoremap <silent> <Plug>Tex_FastCommandInsert i=Tex_DoCommand('no')
nnoremap <silent> <Plug>Tex_FastEnvironmentChange :call Tex_ChangeEnvironments()
vnoremap <silent> <Plug>Tex_FastEnvironmentInsert :call Tex_FastEnvironmentInsert("yes")
nnoremap <silent> <Plug>Tex_FastEnvironmentInsert i=Tex_FastEnvironmentInsert("no")
nmap <F1> <Plug>Tex_Help
nmap <silent> <Plug> i
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
nnoremap <silent> <Plug>CVSWatchRemove :CVSWatch remove
nnoremap <silent> <Plug>CVSWatchOn :CVSWatch on
nnoremap <silent> <Plug>CVSWatchOff :CVSWatch off
nnoremap <silent> <Plug>CVSWatchAdd :CVSWatch add
nnoremap <silent> <Plug>CVSWatchers :CVSWatchers
nnoremap <silent> <Plug>CVSUnedit :CVSUnedit
nnoremap <silent> <Plug>CVSEditors :CVSEditors
nnoremap <silent> <Plug>CVSEdit :CVSEdit
nnoremap <silent> <Plug>VCSVimDiff :VCSVimDiff
nnoremap <silent> <Plug>VCSUpdate :VCSUpdate
nnoremap <silent> <Plug>VCSUnlock :VCSUnlock
nnoremap <silent> <Plug>VCSStatus :VCSStatus
nnoremap <silent> <Plug>VCSSplitAnnotate :VCSAnnotate!
nnoremap <silent> <Plug>VCSReview :VCSReview
nnoremap <silent> <Plug>VCSRevert :VCSRevert
nnoremap <silent> <Plug>VCSLog :VCSLog
nnoremap <silent> <Plug>VCSLock :VCSLock
nnoremap <silent> <Plug>VCSInfo :VCSInfo
nnoremap <silent> <Plug>VCSClearAndGotoOriginal :VCSGotoOriginal!
nnoremap <silent> <Plug>VCSGotoOriginal :VCSGotoOriginal
nnoremap <silent> <Plug>VCSDiff :VCSDiff
nnoremap <silent> <Plug>VCSDelete :VCSDelete
nnoremap <silent> <Plug>VCSCommit :VCSCommit
nnoremap <silent> <Plug>VCSAnnotate :VCSAnnotate
nnoremap <silent> <Plug>VCSAdd :VCSAdd
vmap <silent> <Plug>IMAP_JumpBack `<i=IMAP_Jumpfunc('b', 0)
vmap <silent> <Plug>IMAP_JumpForward i=IMAP_Jumpfunc('', 0)
vmap <silent> <Plug>IMAP_DeleteAndJumpBack "_<Del>i=IMAP_Jumpfunc('b', 0)
vmap <silent> <Plug>IMAP_DeleteAndJumpForward "_<Del>i=IMAP_Jumpfunc('', 0)
nmap <silent> <Plug>IMAP_JumpBack i=IMAP_Jumpfunc('b', 0)
nmap <silent> <Plug>IMAP_JumpForward i=IMAP_Jumpfunc('', 0)
nnoremap <silent> <Plug>CalendarH :cal Calendar(1)
nnoremap <silent> <Plug>CalendarV :cal Calendar(0)
nmap <silent> <F7> :exe ":ptag ".expand("<cword>")
map <silent> <F6> :exe "!evince ". shellescape(expand("%:p:h:t")).".pdf &"
map <silent> <F4> :nohl
map <silent> <F3> :silent make :copen
map <silent> <F2> :silent write
noremap <BS> <PageUp>
noremap <Down> :Tlist 
noremap <Up> :Explore 
noremap <Right> :bn
noremap <Left> :bp 
nmap <C-Space><C-Space>d:vert scs find d =expand("<cword>")
nmap <C-Space><C-Space>i:vert scs find i ^=expand("<cfile>")$
nmap <C-Space><C-Space>e:vert scs find e =expand("<cword>")
nmap <C-Space><C-Space>t:vert scs find t =expand("<cword>")
nmap <C-Space><C-Space>c:vert scs find c =expand("<cword>")
nmap <C-Space><C-Space>g:vert scs find g =expand("<cword>")
nmap <C-Space><C-Space>s:vert scs find s =expand("<cword>")
nmap <C-Space>i :scs find i ^=expand("<cfile>")$
nmap <C-Space>f :scs find f =expand("<cfile>")
nmap <C-Space>e :scs find e =expand("<cword>")
nmap <C-Space>t :scs find t =expand("<cword>")
nmap <C-Space>c :scs find c =expand("<cword>")
nmap <C-Space>d :scs find d =expand("<cword>")
nmap <C-Space>g :scs find g =expand("<cword>")
nmap <C-Space>s :scs find s =expand("<cword>")
imap S <Plug>ISurround
imap s <Plug>Isurround
imap 	 <Plug>Jumper
imap <NL> <Plug>IMAP_JumpForward
imap  <Plug>Isurround
imap jj 
iabbr xuser =g:username 
iabbr ydate =strftime("%B %d, %Y") 
iabbr xdate =strftime("%d/%m/%y %H:%M:%S") 
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set background=dark
set backspace=indent,eol,start
set balloonexpr=SyntasticErrorBalloonExpr()
set browsedir=current
set clipboard=autoselect,exclude:cons\\|linux,unnamed
set cmdheight=2
set complete=.,w,b,u,t,i,k
set confirm
set cscopequickfix=s-,c-,d-,i-,t-,e-
set cscopetag
set cscopeverbose
set noequalalways
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set formatoptions=nrq1
set guicursor=a:blinkon0
set helplang=en
set hidden
set history=1000
set hlsearch
set ignorecase
set incsearch
set iskeyword=a-z,A-Z,_
set laststatus=2
set mouse=a
set pastetoggle=<F5>
set printoptions=paper:letter
set ruler
set runtimepath=~/.vim,~/.vim/bundle/syntastic,~/.vim/bundle/vim-fugitive,~/.vim/bundle/vim-pandoc,~/.vim/bundle/vim-surround,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim73,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after
set scrolloff=10
set shiftwidth=2
set shortmess=atI
set showcmd
set showmatch
set smartcase
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set tabstop=2
set textwidth=72
set viminfo='100,<50,s10,h,!,n~/.vim/viminfo
set visualbell
set whichwrap=b,s,<,>,h,l
set wildignore=*.bak,*.o,*.e,*~
set wildmenu
set wildmode=longest:full,full
" vim: set ft=vim :
