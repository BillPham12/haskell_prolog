The program use a string to check the grey percentage in two sample pictures.
You can use the string " .:-=+*#%@" provided from professor to check, however, this program works with every string.
The longer string input and the more characters are the more probability the program will generate the correct coordinates.

There are some support functions is built up which is getHead (returns head of a list), getTail (return tail of the list)
size (return size of the list), etc. 2 valid functions to check list a contain list b or not (these functions could be minimized by one template function)
And there are 2 counting functions, which will return the index where list a contain b.

TA could use these functions to check how the program perform: (strongly encourage you to use any other string input such as:
"!@#$%^&*()", "<>:{}+_*()#$%#$%#$%$#%$#", etc. they would be anything, it's cool)

checkTheWhole (twoDimension " .:-=+*#%@" True (loadBitmap "sample_image_to_search.bmp"))
(twoDimension " .:-=+*#%@" True (loadBitmap "sample_image_to_find.bmp"))

showAsASCIIArt(twoDimension " .:-=+*#%@" True (loadBitmap "sample_image_to_search.bmp"))
showAsASCIIArt(twoDimension " .:-=+*#%@" True (loadBitmap "sample_image_to_find.bmp"))
