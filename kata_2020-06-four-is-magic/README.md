# Four is Magic

## Task

Write a function/method/class that takes a number and returns an English text sequence starting with the English cardinal representation of that integer, the word 'is' and then the English cardinal representation of the count of characters that made up the first word, followed by a comma.
Continue the sequence by using the previous count word as the first word of the next phrase, append 'is' and the cardinal count of the letters in that word.
Continue until you reach four. 
Since four has four characters, finish by adding the words 'four is magic' and a period. 
All integers will eventually wind up at four.

### Example

For instance, suppose your are given the integer **3**. 
Convert **3** to **Three**, add **is** , then the cardinal character count of three, which is **five**, with a comma (**,**) to separate it from the next phrase. 
Continue the sequence **five is four**, (five has four letters), and finally, **four is magic**.
```
Three is five, five is four, four is magic.
```

### More Examples

```
Just "Five is four, four is magic."

Just "Ten is three, three is five, five is four, four is magic."

Just "Seventeen is nine, nine is four, four is magic."

Just "Thirty-two is ten, ten is three, three is five, five is four, four is magic."

Just "One thousand nine hundred forty-three is thirty-seven, thirty-seven is twelve, twelve is six, six is three, three is five, five is four, four is magic."

Just "Thirteen thousand nine hundred fifty-four is forty-one, forty-one is nine, nine is four, four is magic."

Just "One trillion two hundred thirty-four million forty-five thousand two hundred thirty-four is eighty-eight, eighty-eight is twelve, twelve is six, six is three, three is five, five is four, four is magic."

Just "One hundred twenty-three quadrillion four hundred ninety-five trillion three hundred twenty-four million three hundred twenty-four thousand three hundred twenty is one hundred sixty, one hundred sixty is seventeen, seventeen is nine, nine is four, four is magic."

Just "One hundred eleven septillion two hundred twenty-two sextillion three hundred thirty-three quintillion four hundred forty-four quadrillion five hundred fifty-five trillion six hundred sixty-six million seven hundred seventy-seven thousand eight hundred eighty-eight is two hundred sixty-five, two hundred sixty-five is twenty-two, twenty-two is ten, ten is three, three is five, five is four, four is magic."
```

## Guidelines

* You may assume the input will only contain integer numbers.
* Cardinal numbers between 20 and 100 may use either hyphens or spaces as word separators but they must use a word separator. (23 is twenty three or twenty-three not twentythree.)
* Cardinal number conversions should follow the English short scale. (billion is 1e9, trillion is 1e12, etc.)
* Cardinal numbers should not include commas. (20140 is twenty thousand one hundred forty not twenty thousand, one hundred forty.)
* When converted to a string, 100 should be one hundred, not a hundred or hundred, 1000 should be one thousand, not a thousand or thousand.
* When converted to a string, there should be no and in the cardinal string. 130 should be one hundred thirty not one hundred and thirty.
* When counting characters, count all of the characters in the cardinal number including spaces and hyphens. One hundred fifty-one should be 21 not 18.
* The output should follow the format "N is K, K is M, M is ... four is magic." (unless the input is 4, in which case the output should simply be "four is magic.")
* The output can either be the return value from the function, or be displayed from within the function.
* You are encouraged, though not mandated to use proper sentence capitalization.
* You may optionally support negative numbers. -7 is negative seven.

# Source
https://rosettacode.org/wiki/Four_is_magic
