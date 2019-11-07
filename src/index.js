import './reset.css';
import './KenVectorFuture.ttf';
import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

import iconMoneybag from './icons/1f4b0.svg'
import iconPage from './icons/1f4c4.svg'
import iconMailbox from './icons/1f4eb.svg'
import iconBlondeFace from './icons/1f9d1.svg'
import iconBlackhairedFace from './icons/1f9d1-1f3fb.svg'
import iconRedhairedFace from './icons/1f9d1-1f3fd.svg'
import iconKeyboard from './icons/2328.svg'
import iconEnvelope from './icons/2709.svg'

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    icons: [
      ["moneybag", iconMoneybag],
      ["page", iconPage],
      ["mailbox", iconMailbox],
      ["blondeface", iconBlondeFace],
      ["blackhairedface", iconBlackhairedFace],
      ["redhairedface", iconRedhairedFace],
      ["keyboard", iconKeyboard],
      ["envelope", iconEnvelope],
    ]
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
