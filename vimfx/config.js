/* global vimfx */

vimfx.set('hint_chars', 'asdfjkl');
vimfx.set('ignore_keyboard_layout', true);

vimfx.addKeyOverrides(
  [ (location) => location.host === 'www.facebook.com',
    'jkl'.split('')
  ],
  [ (location) => location.host === 'inbox.google.com',
    'jkcnp/ie[]raf#?!xs'.split('')
  ],
  [ (location) => location.host === 'outlook.office.com',
    '?gijkxcraf#e'.split('')
  ],
  [ (location) => location.host === 'tweetdeck.twitter.com',
    'jklhrlfg?'.split('')
  ]
);

let map = (shortcuts, command, custom=false) => {
  vimfx.set(`${custom ? 'custom.' : ''}mode.normal.${command}`, shortcuts);
};

function disable_cmd ( cmd )	{
  map('', cmd);
}

//disable a-prefixed cmds
[
  'reload_all',
].map(disable_cmd);

map('<a-l>', 'tab_select_next');
map('<a-h>', 'tab_select_previous');
map('<a-tab>', 'tab_select_most_recent');
map('<c-i>', 'history_back');
map('<c-o>', 'history_forward');
map('d', 'tab_close');
map('u', 'tab_restore');
map('<c-d>','scroll_half_page_down');
map('<c-u>','scroll_half_page_up');
