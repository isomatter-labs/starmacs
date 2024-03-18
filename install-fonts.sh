#!/bin/bash

font_src_dir="$(pwd)/fonts"

find_command="find \"$font_src_dir\" \( -name '*.[o,t]t[f,c]' -or -name '*.pcf.gz' \) -type f -print0"

if [[ `uname` == 'Darwin' ]]; then
  # MacOS
  target_dir="$HOME/Library/Fonts"
else
  # Linux
  target_dir="$HOME/.local/share/fonts"
  mkdir -p $target_dir
fi

echo -e "Run: $find_command | xargs -0 -I % cp \"%\" \"$target_dir/\"\n"

# Copy all fonts to user fonts directory
echo "Copying fonts..."
# printing
eval $find_command | xargs -0 -I %

eval $find_command | xargs -0 -I % cp "%" "$target_dir/"

# Reset font cache on Linux
if command -v fc-cache @>/dev/null ; then
    echo -e "\nResetting font cache, this may take a moment..."
    fc-cache -f $target_dir
fi

echo -e "\nAll fonts have been installed to $target_dir"

