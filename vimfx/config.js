/* global vimfx */

vimfx.set('hint_chars', 'asdfjkl');

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

map('<a-j>', 'tab_select_next');
map('<a-k>', 'tab_select_previous');
map('<a-tab>', 'tab_select_most_recent');
map('<a-h>', 'history_back');
map('<a-l>', 'history_forward');
