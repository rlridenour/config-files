function hidehidden
    defaults write com.apple.finder AppleShowAllFiles FALSE
	killall Finder
end