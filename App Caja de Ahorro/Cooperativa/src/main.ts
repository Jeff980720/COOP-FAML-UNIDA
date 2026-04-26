import { bootstrapApplication } from '@angular/platform-browser';
import { appConfig } from './app/app.config';
import { App } from './app/app';
import { InicioComponent } from './app/components/inicio-component/inicio-component';

bootstrapApplication(App, appConfig)
  .catch((err) => console.error(err));
