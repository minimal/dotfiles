{lib}: {
  reverseString = s:
    lib.concatStrings (lib.reverseList (lib.stringToCharacters s));
}