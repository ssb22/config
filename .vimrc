" -*- mode: vimrc -*-
" Silas S. Brown's .vimrc file and vim notes

" Autocomplete (in insert mode) : Ctrl-N

" Search fwd/backward           : /,?  (like in less)
" Repeat forward,backward search: n,N  (also in less)
" Go to line 47                 : 47G  (also in less)
" end-of-buffer (M->)           : G    (also in less)
" C-a, C-e (start/end line)     : ^,$
" Select,cut,copy,paste         : v,x,y,p
" Undo,redo                     : u,Ctrl-r
" manual-entry                  : K

" search/replace regex through whole file, with confirm:
" :%s/search/replace/gc  (y,n,a, l=last, q=cancel)

" fill-paragraph                : gwip

" Window splitting etc:
" Ctrl-w s (:sp) split window vertically (Emacs C-x 2)
" Ctrl-w Ctrl-w = switch window (Emacs C-x o)
" Ctrl-w q (:q) = close window
" Ctrl-w +/-/= to change or equalise window sizes

" :e filename (has tab completion)
"   (or switches buffer if it's open in another window)
" :sp filename (w.tab) splits window and opens
" :3sp [filename] new 3-line window
" Can open a directory to get a dir listing

" :tabe filename , gt , gT = tab switch (layouts)

" or if load multiple files from command line,
" :n,:N = switch, :args = current filename
" (use vim -p to open them in tabs instead)

" Where to find history of this file:
" on GitHub at https://github.com/ssb22/config
" and on GitLab at https://gitlab.com/ssb22/config
" and on BitBucket https://bitbucket.org/ssb22/config
" and at https://gitlab.developers.cam.ac.uk/ssb22/config
" and in China: https://gitee.com/ssb22/config

:syntax on  " syntax highlighting

:set autoindent " (use   :set paste   to turn off)
:set smartindent
:set cindent
:set shiftwidth=4
:set expandtab

:set incsearch " incremental search with / and ?
:set hlsearch  " highlight all matches
:set smartcase " use case if any caps used
" (:set ignorecase, :set noignorecase)

:set textwidth=0 " (use screen width to get text width)
map <Space> <PageDown>
" (like 'less' etc)
:set hidden " keep hidden buffers loaded
:set backspace=2
:set ruler " show cursor pos
