// an example to create a new mapping `ctrl-y`

const { mapkey, map, unmap } = api;

mapkey("<Ctrl-y>", "Show me the money", function () {
  Front.showPopup(
    "a well-known phrase uttered by characters in the 1996 film Jerry Maguire (Escape to close)."
  );
});

// an example to replace `T` with `gt`, click `Default mappings` to see how `T` works.
map("<Ctrl-u>", "u");
map("<Ctrl-d>", "d");
unmap("d");
map("d", "x");
map("<Alt-l>", "R");
map("<Alt-h>", "E");
map("L", "D");
map("H", "S");
settings.tabsThreshold = 0;
map("u", "X");

// removeSearchAliasX("w");
// addSearchAliasX(
//   "w",
//   "Wikipedia",
//   "https://en.wikipedia.org/w/index.php?search=",
//   "s",
//   "https://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=",
//   function (response) {
//     var res = JSON.parse(response.text);
//     Omnibar.listWords(res[1]);
//   }
// );
// removeSearchAliasX("b");

if (window.location.origin === "https://mail.google.com") {
  unmap("j");
  unmap("k");
  unmap("x");
  unmap("c");
  unmap("r");
  unmap("m");
}
settings.modeAfterYank = "Normal";
settings.showModeStatus = true;
settings.hintShiftNonActive = true;

settings.theme =
  "\
#sk_omnibar {\
  width: 100%;\
  top: 0px;\
  left: 0;\
}\
";

if (window.location.origin === "https://meet.google.com") {
  unmap("<Ctrl-d>");
}
