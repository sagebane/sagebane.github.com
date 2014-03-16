---
layout: post
title: emacs 配置
---
## org-mode

### 导出到中文pdf
org-mode 8.0以上，需要用到ox-latex中的变量：

	(require 'ox-latex)

配置xelatex引擎处理tex为pdf：

	(setq org-latex-pdf-process '("xelatex -shell-escape -pdf -quiet %f"
			      "xelatex -shell-escape -pdf -quiet %f"))

采用xelatex引擎，默认为utf-8编码，去掉缺省包列表里面的inputenc配置:

	(setf org-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
	(setf org-latex-default-packages-alist
      (remove '("T1" "fontenc" t) org-latex-default-packages-alist))

中文字体配置：

	(setq org-latex-packages-alist
       '("\\usepackage{fontspec}
	\\XeTeXlinebreaklocale ``zh''
	\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt
	\\newcommand\\fontnamehei{WenQuanYi Zen Hei}
	\\newcommand\\fontnamesong{AR PL UMing CN}
	\\newcommand\\fontnamekai{AR PL KaitiM Big5}
	\\newcommand\\fontnamemono{FreeMono}
	\\newcommand\\fontnameroman{FreeSans}
	\\setmainfont[BoldFont=\\fontnamehei]{\\fontnamesong}
	\\setsansfont[BoldFont=\\fontnamehei]{\\fontnamekai}
	\\setmonofont{\\fontnamemono}"))

### 代码高亮
安装python提供的语法分析模块pygments:

	sudo pip install pygments

在init.el中添加支持：

	(setq org-export-latex-listings t)
	(add-to-list 'org-latex-packages-alist '("" "minted"))
	(setq org-latex-listings 'minted)

生成pdf的指令修改为:

	(setq org-latex-pdf-process '("xelatex -shell-escape -pdf -quiet %f"
		"xelatex -shell-escape -pdf -quiet %f"))

C-c C-e l o编译，产生错误：

![org-mode error](/images/orgerror.png)

这是某个版本minted宏包的bug，如下图，将其查找指令的方式，由which -s 改
为which -a即可。

![minted bug](/images/minted-bug.png)

### emcs字典

安装星际译王：

    apt-get install stardict sdcv

在这里<http://stardict.abloz.com> 下载字典，并按提示方法安装。

在init.el中增加代码：

    (global-set-key (kbd "C-c d") 'kid-sdcv-to-buffer)
    (defun kid-sdcv-to-buffer ()
		(interactive)
		(let ((word (if mark-active
                      (buffer-substring-no-properties (region-beginning) (region-end))
                      (current-word nil t))))
        (setq word (read-string (format "Search the dictionary for (default %s): " word)
                                nil nil word))
        (set-buffer (get-buffer-create "*sdcv*"))
        (buffer-disable-undo)
        (erase-buffer)
        (let ((process (start-process-shell-command "sdcv" "*sdcv*" "sdcv" "-n" word)))
          (set-process-sentinel
           process
           (lambda (process signal)
             (when (memq (process-status process) '(exit signal))
               (unless (string= (buffer-name) "*sdcv*")
                 (setq kid-sdcv-window-configuration (current-window-configuration))
                 (switch-to-buffer-other-window "*sdcv*")
                 (local-set-key (kbd "d") 'kid-sdcv-to-buffer)
                 (local-set-key (kbd "q") (lambda ()
                                            (interactive)
                                            (bury-buffer)
                                            (unless (null (cdr (window-list))) ; only one window
                                              (delete-window)))))
               (goto-char (point-min))))))))

使用快捷键C-c d查字典。

### 其它
doxymacs 非常方便配合doxygen生成api文档
将el编译为elc，加速emacs装载:

	byte-compile-file
## 互换caps lock 和 Ctrl

编辑文件~/.Xmodmap：

	!
	! Swap Caps_Lock and Control_L
	!
	remove Lock = Caps_Lock
	remove Control = Control_L
	keysym Control_L = Caps_Lock
	keysym Caps_Lock = Control_L
	add Lock = Caps_Lock
	add Control = Control_L

执行：

	xmodmap ~/.Xmodmap



## sawfish 配置
    (require 'sawfish-defaults)
    
    (custom-set-typed-variable (quote wm-modifier-value) 
    			   (quote (super)) 
    			   (quote modifier-list))
    (custom-set-typed-variable (quote default-frame-style)
    			   (quote brushed-metal)
    			   (quote frame-style))
    (custom-set-typed-variable (quote workspace-names)
    			   (quote ("local" "remote" "other" "virtual"))
    			   (quote (list string "virtual desk top")))
    
    (custom-set-typed-variable (quote focus-mode)
    			   (quote enter-only)
    			   (quote symbol))
    (custom-set-typed-variable (quote place-window-mode)
    			   (quote first-fit)
    			   (quote symbol))
    
    ;; this function is used a bit further in, in my local config. i use it to blur
    ;; the line between what's running and what needs to be started, so i can hit a
    ;; key to load something, or jump to it if it was already running. i love
    ;; sawfish.
    (defun jump-or-exec (re prog #!optional onfocused)
      "jump to a window matched by re, or start program otherwise."
      (catch 'return
        (let ((wind (get-window-by-name-re re)))
          (if (functionp onfocused) ; check if already focused
              (let ((curwin (input-focus)))
                (if curwin
                    (if (string-match re (window-name curwin))
                        (progn
                          (funcall onfocused wind)
                          (throw 'return))))))
          (if (windowp wind)
              (display-window wind)
            (if (functionp prog)
                (funcall prog)
              (system (concat prog "&")))))))
    
    
    ;; start up
    (define startup-programs
      '(
        ("feh" "--bg-center" "/home/sgbn/img/wallpaper/sundown-600x900.jpg")
        ))
    
    (mapc (lambda (program)
    	(apply start-process (make-process standard-output) program))
          startup-programs)
    
    (add-hook 'before-exit-hook 
    	  (lambda () 
    	    (mapc stop-process (active-processes))))
    
    ;; global manipulations
    (bind-keys global-keymap
    	   ;; system relative
    	   "W-o" 'restart
    	   "W-F12" '(run-shell-command "xscreensaver-command -lock")
    
    	   ;; active worksapce
    	   "W-1" '(activate-workspace 1)
    	   "W-2" '(activate-workspace 2)
    	   "W-3" '(activate-workspace 3)
    	   "W-4" '(activate-workspace 4)
    
    	   ;;jump or exec
    	   "W-e" `(jump-or-exec "emacs " "emacs")
    	   "W-f" `(jump-or-exec "Pentadactyl" "firefox")
    	   "W-RET" `(jump-or-exec "Terminal" "gnome-terminal -e screen")
    	   "W-s" `(jump-or-exec "StarDict" "stardict")
    )
    
    ;; Window manipulations
    (bind-keys window-keymap
               ;; Grow to an edge
               "C-W-k" 'grow-window-up
               "C-W-j" 'grow-window-down
               "C-W-h" 'grow-window-left
               "C-W-l" 'grow-window-right
    
               ;; Move to an edge
    	   "W-k" 'pack-window-up
    	   "W-j" 'pack-window-down
    	   "W-h" 'pack-window-left
    	   "W-l" 'pack-window-right
    
    	   ;; move window to worksapce
    	   "W-!" '(send-to-workspace 1)
    	   "W-@" '(send-to-workspace 2)
    	   "W-$" '(send-to-workspace 4)
    	   "W-#" '(send-to-workspace 3)
    
    	   ;; maxmize the window
    	   "W-m" 'maximize-fill-window-toggle
    	   "W-X" '(delete-window (current-event-window)))
    
