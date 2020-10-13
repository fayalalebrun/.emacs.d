(defun fal-vhdl-ghdl-ae()
  (interactive)
  (if (not (file-exists-p "work"))
      (make-directory "work"))
  (shell-command (concat"ghdl -a " buffer-file-name))
  (shell-command (concat"ghdl -e " (file-name-nondirectory (file-name-sans-extension buffer-file-name))))
  (shell-command (concat "ghdl -r " (file-name-nondirectory (file-name-sans-extension buffer-file-name)) "  --wave=wave.ghw"))
  
  (async-shell-command "gtkwave wave.ghw")
  )