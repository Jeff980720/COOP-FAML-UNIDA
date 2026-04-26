import { Routes } from '@angular/router';
import { Login } from './components/login/login';
import { InicioComponent } from './components/inicio-component/inicio-component';
import { SociosComponent } from './components/socios-component/socios-component';
import { EstadoResultado } from './components/estado-resultado/estado-resultado'; // Ajusta si el nombre varía
import { AddSocioComponent } from './components/add-socio-component/add-socio-component';
import { AddAporteComponent } from './components/add-aporte-component/add-aporte-component';
import { AddPagosComponent } from './components/add-pagos-component/add-pagos-component';
import { AddPrestamosComponent } from './components/add-prestamos-component/add-prestamos-component';
import { CajaComponent } from './components/caja-component/caja-component';
import { ListaAporteComponent } from './components/lista-aporte-component/lista-aporte-component';
import { ListaPrestamosComponent } from './components/lista-prestamos-component/lista-prestamos-component';
import { ListaPagosComponent } from './components/lista-pagos-component/lista-pagos-component';
import { ListaCajaComponent } from './components/lista-caja-component/lista-caja-component';
import { ListaPagoPorSocio } from './components/lista-pago-por-socio/lista-pago-por-socio';
import { ListaPrestamoPorSocio } from './components/lista-prestamo-por-socio/lista-prestamo-por-socio';
import { Configuracion } from './components/configuracion/configuracion';
import { Dashboard } from './components/dashboard/dashboard';

export const routes: Routes = [
  // 1. La ruta de login
  { path: 'login', component: Login },

  // 2. Redirección inicial: Si entran a la raíz, VAN AL LOGIN
  { path: '', redirectTo: 'login', pathMatch: 'full' },

  // 3. Rutas de la aplicación
  { path: 'inicio', component: InicioComponent },
  { path: 'socios', component: SociosComponent },
  { path: 'estado-resultados', component: EstadoResultado },
  { path: 'add-socio', component: AddSocioComponent },
  { path: 'editar-socio/:id', component: AddSocioComponent },
  { path: 'aportes', component: AddAporteComponent },
  { path: 'editar-aporte/:id', component: AddAporteComponent },
  { path: 'pagos', component: AddPagosComponent },
  { path: 'editar-pagos/:id', component: AddPagosComponent },
  { path: 'prestamos', component: AddPrestamosComponent },
  { path: 'editar-prestamos/:id', component: AddPrestamosComponent },
  { path: 'caja', component: CajaComponent },
  { path: 'editar-caja/:id', component: CajaComponent },
  { path: 'listaportes', component: ListaAporteComponent },
  { path: 'litaprestamos', component: ListaPrestamosComponent },
  { path: 'listapagos', component: ListaPagosComponent },
  { path: 'listacaja', component: ListaCajaComponent },
  { path: 'pagosdelsocio/:id', component: ListaPagoPorSocio },
  { path: 'prestamosdelsocio/:id', component: ListaPrestamoPorSocio },
  { path: 'dashboard', component: Dashboard },
  { path: 'configuracion', component: Configuracion },

  // 4. Comodín: Si escriben cualquier cosa mal, al login (o al inicio si prefieres)
  { path: '**', redirectTo: 'login' }
];
// import { Routes } from '@angular/router';
// import { InicioComponent } from './components/inicio-component/inicio-component';
// import { SociosComponent } from './components/socios-component/socios-component';
// import { AddSocioComponent } from './components/add-socio-component/add-socio-component';
// import { AddAporteComponent } from './components/add-aporte-component/add-aporte-component';
// import { AddPagosComponent } from './components/add-pagos-component/add-pagos-component';
// import { AddPrestamosComponent } from './components/add-prestamos-component/add-prestamos-component';
// import { CajaComponent } from './components/caja-component/caja-component';
// import { ListaAporteComponent } from './components/lista-aporte-component/lista-aporte-component';
// import { ListaPrestamosComponent } from './components/lista-prestamos-component/lista-prestamos-component';
// import { ListaPagosComponent } from './components/lista-pagos-component/lista-pagos-component';
// import { ListaCajaComponent } from './components/lista-caja-component/lista-caja-component';
// import { ListaPagoPorSocio } from './components/lista-pago-por-socio/lista-pago-por-socio';
// import { ListaPrestamoPorSocio } from './components/lista-prestamo-por-socio/lista-prestamo-por-socio';
// import { EstadoResultado } from './components/estado-resultado/estado-resultado';
// import { Login } from './components/login/login';

// /**
//  * Este es el "mapa" de navegación.
//  */
// export const routes: Routes = [
//   {
//     path: 'login', component: Login

//   },
//   {
//     path: '', redirectTo: 'login', pathMatch: 'full'

//   }, // Si entran a la raíz, redirige a login
//   {
//     path: 'inicio',
//     component: InicioComponent
//   },
//   {
//     path: 'socios',
//     component: SociosComponent
//   },
//   {
//     path: 'estado-resultados',
//     component: EstadoResultado
//   },
//   {
//     path: 'editar-socio/:id',
//     component: AddSocioComponent
//   },
//   {
//     path: 'aportes',
//     component: AddAporteComponent
//   },

//   {
//     path: 'editar-aporte/:id',
//     component: AddAporteComponent
//   },
//   {
//     path: 'pagos',
//     component: AddPagosComponent
//   },
//   {
//     path: 'editar-pagos/:id',
//     component: AddPagosComponent
//   },
//   {
//     path: 'prestamos',
//     component: AddPrestamosComponent
//   },
//   {
//     path: 'editar-prestamos/:id',
//     component: AddPrestamosComponent
//   },
//   {
//     path: 'caja',
//     component: CajaComponent
//   },
//   {
//     path: 'editar-caja/:id',
//     component: CajaComponent
//   },
//   {
//     path: 'add-socio',
//     component: AddSocioComponent
//   },
//   {
//     path: 'listaportes',
//     component: ListaAporteComponent
//   },
//   {
//     path: 'litaprestamos',
//     component: ListaPrestamosComponent
//   },
//   {
//     path: 'listapagos',
//     component: ListaPagosComponent
//   },
//   {
//     path: 'listacaja',
//     component: ListaCajaComponent
//   },
//   {
//     path: 'pagosdelsocio/:id',
//     component: ListaPagoPorSocio
//   },
//   {
//     path: 'prestamosdelsocio/:id',
//     component: ListaPrestamoPorSocio
//   },
//   {
//     path: '', // Si la URL está vacía
//     redirectTo: '/inicio', // Redirige a /inicio
//     pathMatch: 'full'
//   },
//   {
//     path: '**', // Si la URL no coincide con nada (Error 404)
//     redirectTo: '/inicio',
//     pathMatch: 'full'
//   },
// ];
