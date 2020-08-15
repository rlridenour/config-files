function draft -d "Open default editor with date prepended filename and specified title for jekyll posts."

cd ~/Dropbox/blog-drafts

# Create file
  set title (date +"%Y-%m-%d")
  
  switch (count $argv) 
    case 0
      echo "No post title was specified."
      return 1
    case 1
      set lower_arg (echo $argv[1] | tr A-Z a-z)
      set title $title-$lower_arg.md
    case \*
      for arg in $argv
        set lower_arg (echo $arg | tr A-Z a-z)
        set title $title-$lower_arg
      end
      set title $title.md
  end
  
# Append YAML
echo "---" >> $title
echo "layout: post" >> $title
echo "title: " $argv >> $title
echo "tags:" >> $title
echo "- " >> $title
echo "comments: true" >> $title
echo "date:" (date +"%Y-%m-%d %H:%M:%S") >> $title
echo "---" >> $title

# Open file in VIM
  EDITOR $title
end
