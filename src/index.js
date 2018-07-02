import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.fullscreen();

registerServiceWorker();

window.addEventListener('blur', () => {
    app.ports.windowBlur.send('');
});
