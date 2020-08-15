function showhidden
    defaults write com.apple.finder AppleShowAllFiles TRUE
	killall Finder
end