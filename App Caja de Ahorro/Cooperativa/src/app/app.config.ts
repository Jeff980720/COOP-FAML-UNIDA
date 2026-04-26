import { ApplicationConfig, provideBrowserGlobalErrorListeners, provideZoneChangeDetection } from '@angular/core';
import { provideRouter } from '@angular/router';

// Importa las rutas que acabamos de definir
import { routes } from './app.routes';
import { provideHttpClient, withInterceptors } from '@angular/common/http';
import { MAT_DATE_LOCALE } from '@angular/material/core';
import { provideNativeDateAdapter } from '@angular/material/core';
import { authInterceptor } from './interceptors/auth-interceptor'; // Importa tu función
import { provideCharts, withDefaultRegisterables } from 'ng2-charts';
// export const appConfig: ApplicationConfig = {
//   providers: [
//     provideBrowserGlobalErrorListeners(),
//     provideZoneChangeDetection({ eventCoalescing: true }),
//     provideRouter(routes)// ¡Aquí le decimos a Angular que use nuestras rutas!
//     // otros providers si los tienes...
//   ]
// };

export const appConfig: ApplicationConfig = {
  providers: [
    provideZoneChangeDetection({ eventCoalescing: true }),
    provideRouter(routes),
    provideHttpClient(
      withInterceptors([authInterceptor])
    ),  // Habilita HttpClient
    provideNativeDateAdapter(),
    { provide: MAT_DATE_LOCALE, useValue: 'es-EC' }, // Configura idioma Ecuador
    // Esto fuerza a que no se use el desfase horario al serializar
    provideCharts(withDefaultRegisterables()) // <-- Aquí es donde se activan los gráficos ahora
  ]
};
