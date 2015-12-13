hlint src \
&& strip --strip-all --remove-section=.comment --remove-section=.note ~/.local/bin/saha \
&& strip --strip-all --remove-section=.comment --remove-section=.note ~/.local/bin/saha-server \
&& upx -9 ~/.local/bin/saha \
&& upx -9 ~/.local/bin/saha-server
