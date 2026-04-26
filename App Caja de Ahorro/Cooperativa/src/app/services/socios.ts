import { Injectable } from '@angular/core';
import { Coperativa } from '../models/Coperativa';
import { catchError, Observable, of, throwError } from 'rxjs';
import { Socio } from '../models/Socio';
import { Aporte } from '../models/Aportes';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { map } from 'rxjs/operators';
import { tap } from 'rxjs/operators';
import { Prestamos } from '../models/Prestamos.js';
import { Pagos } from '../models/Pagos.js';
import { Caja } from '../models/Caja';
import { AuthResponse } from '../models/Usuarios';
import { Router } from '@angular/router';

@Injectable({ providedIn: 'root' })
// export class FiltroDataService {
//   // Aquí guardaremos el estado del filtro
//   filterState = {
//     idSocio: null,
//     nombreSocio: '',
//     fechaInicio: null,
//     fechaFin: null
//   };

//   constructor() { }
// }
export class SociosService {
  private apiUrl = 'http://localhost:3000/api';
  // Variables persistentes

  public fechaInicioOperacion: string = '';
  public fechaInicioPeriodo: string = '';
  public fechaFinPeriodo: string = '';

  public fechaInicioPeriodo1: string = '';
  public fechaFinPeriodo1: string = '';

  public fechaInicioPeriodo2: string = '';
  public fechaFinPeriodo2: string = '';

  public fechaInicioPeriodo3: string = '';
  public fechaFinPeriodo3: string = '';

  // Datos que vendrán de tus servicios de Socios y Pagos
  public estadoResultado = {
    dineroAportado: 0.00,
    dineroEnPrestamo: 0.00,
    dineroAyuda: 0.00,
    dineroPrestadoMensual: 0.00,
    interesEnPrestamo: 0.00,
    interesPrestadoMensual: 0.00,
    dineroPaga: 0.00,
    dineroPagadoMensual: 0.00,
    dineroPorCobrar: 0.00,
    interesPagado: 0.00,
    interesPagadoMensual: 0.00,
    interesPorCobrar: 0.00,
    ingresosVarios: 0.00,
    egresosSocios: 0.00,
    egresoInteresAnulado: 0.00,
    totalAportados: 0.00,
    aporteIndividual: 0.00,
    interesGeneradoReal: 0.00,
    dineroEnCaja: 0.00,
    dineroGenerado: 0.00,
    verificacion: 0.00
  };

  public resultadoDelMes = {
    dineroAportado: 0.00,
    dineroPrestado: 0.00,
    dineroPagadoEfectivo: 0.00,
    dineroPagadoReprestamo: 0.00,
    interesPrestado: 0.00,
    interesPagado: 0.00,
    dineroCajaAnterior: 0.00,
    recaudado: 0.00,
    prestado: 0.00,
    dineroCajaActual: 0.00,
  }

  constructor(private http: HttpClient, private router: Router) { }

  //************************* SERVICIOS PARA CÁLCULOS  */
  /**
   * Obtiene la lista de socios con todos los cálculos interactivos:
   * # Aportes, Ingresos, Gastos, Ganado y Total.
   */
  // getResumenCompleto(): Observable<any[]> {
  //   return this.http.get<any[]>(`${this.apiUrl}/resumen-dinamico`);
  // }

  //******************** SERVICIO PARA SOCIOS */
  //1. OBTENER SOCIOS
  // getAllSocios(): Observable<Socio[]> {
  //   return this.http.get<Socio[]>(`${this.apiUrl}/socios`).pipe(
  //     map(data => data.filter(s => s.idsocio !== '01' && s.idsocio !== '02')),
  //     catchError(this.handleError<Socio[]>('getSociosParaPagos', []))
  //   );
  // }

  // login(username: string, password: string): Observable<boolean> {
  //   return this.http.post<AuthResponse>(`${this.apiUrl}/auth/login`, { username, password })
  //     .pipe(
  //       tap(resp => {
  //         if (resp.ok) {
  //           // Guardamos el token y datos básicos para la sesión
  //           localStorage.setItem('token', resp.token);
  //           localStorage.setItem('rol', resp.rol);
  //           localStorage.setItem('nombre', resp.nombre);
  //         }
  //       }),
  //       map(resp => resp.ok),
  //       catchError(err => of(false)) // Si falla (400 o 500), devolvemos false
  //     );
  // }

  login(username: string, password: string): Observable<AuthResponse> {
    return this.http.post<AuthResponse>(`${this.apiUrl}/auth/login`, { username, password })
      .pipe(
        tap(resp => {
          if (resp.ok) {
            localStorage.setItem('token', resp.token);
            localStorage.setItem('rol', resp.rol);
            localStorage.setItem('nombre', resp.nombre);
          }
        })
        // Quitamos el map y el catchError de aquí
      );
  }

  logout() {
    localStorage.removeItem('token');
    localStorage.removeItem('rol');
    localStorage.removeItem('nombre');
    this.router.navigateByUrl('/login');
  }

  // En socios.ts (Debajo de getSocios)
  getUsuarios(): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/usuarios`).pipe(
      catchError(err => {
        console.error('Error al obtener usuarios', err);
        return of([]);
      })
    );
  }

  // src/app/services/socios.ts

  // crearUsuario(usuario: any): Observable<any> {
  //   return this.http.post(`${this.apiUrl}/usuarios`, usuario);
  // }

  // actualizarUsuario(id: number, usuario: any): Observable<any> {
  //   return this.http.put(`${this.apiUrl}/usuarios/${id}`, usuario);
  // }
  crearUsuario(usuarioConClave: any): Observable<any> {
    // usuarioConClave contendrá { nombre, rol, ..., adminPassword }
    return this.http.post(`${this.apiUrl}/usuarios`, usuarioConClave);
  }

  actualizarUsuario(id: number, usuarioConClave: any): Observable<any> {
    return this.http.put(`${this.apiUrl}/usuarios/${id}`, usuarioConClave);
  }
  // eliminarUsuario(id: number): Observable<any> {
  //   return this.http.delete(`${this.apiUrl}/usuarios/${id}`);
  // }
  eliminarUsuario(id: number, contrasenia: string) {
    return this.http.delete(`${this.apiUrl}/usuarios/${id}`, { body: { contrasenia } });
  }

  //1.4 OBTENER SOCIOS PARA COOPERATIVA
  getSocios(): Observable<Coperativa> {
    // La ruta es /api/socios
    // return this.http.get<Coperativa>(`${this.apiUrl}/socios-reporte`).pipe(
    return this.http.get<Coperativa>(`${this.apiUrl}/socios/reporte`).pipe(
      // Añadir manejo de errores es una buena práctica
      catchError(this.handleError<Coperativa>('getSocios', [] as unknown as Coperativa))
    );
  }

  getAllSocios(): Observable<Socio[]> {
    return this.http.get<Socio[]>(`${this.apiUrl}/socios`).pipe(
      map(data => data.filter(s => s.idsocio !== '01' && s.idsocio !== '02')),
      catchError(this.handleError<Socio[]>('getSociosParaPagos', []))
    );
  }

  // 1.1 OBTENER SOCIOS PARA PAGOS
  getSociosParaPagos(): Observable<Socio[]> {
    return this.http.get<Socio[]>(`${this.apiUrl}/socios`).pipe(
      map(data => data.filter(s => s.idsocio !== '01' && s.idsocio !== '02')),
      catchError(this.handleError<Socio[]>('getSociosParaPagos', []))
    );
  }

  // 1.2 OBTENER SOCIOS PARA INGRESOS
  getSociosParaIngresos(): Observable<Socio[]> {
    return this.http.get<Socio[]>(`${this.apiUrl}/socios`).pipe(
      map(data => data.filter(s => s.idsocio !== '02')),
      catchError(this.handleError<Socio[]>('getSociosParaIngresos', []))
    );
  }

  // 1.3 OBTENER SOCIOS PARA EGRESOS
  getSociosParaEgresos(): Observable<Socio[]> {
    return this.http.get<Socio[]>(`${this.apiUrl}/socios`).pipe(
      map(data => data.filter(s => s.idsocio !== '01')),
      catchError(this.handleError<Socio[]>('getSociosParaEgresos', []))
    );
  }

  //2. OBTENER SIGUIENTE ID PARA NUEVO SOCIO
  getSiguienteIdSocio(): Observable<{ siguienteId: string }> {
    return this.http.get<{ siguienteId: string }>(`${this.apiUrl}/socios/siguiente-id`);
  }

  //3. REGISTRAR UN NUEVO SOCIO
  registrarSocio(nuevoSocio: Socio): Observable<Socio> {
    // return this.http.post<Socio>(`${this.apiUrl}/socios`, nuevoSocio).pipe(
    return this.http.post<Socio>(`${this.apiUrl}/socios/guardar`, nuevoSocio).pipe(
      tap((res: Socio) => console.log('Socio guardado con éxito:', res)),
      catchError((error) => {
        // Este error suele ser el 500 si idsocio es un objeto o idaporte ya existe
        console.error('Error en registrarSocio:', error);
        throw error;
      })
    );
  }

  //4. OBTENER UN SOCIO POR ID
  getSocioById(id: string): Observable<Socio> {
    return this.http.get<Socio>(`${this.apiUrl}/socios/${id}`).pipe(
      catchError(this.handleError<Socio>(`getSocioById id=${id}`))
    );
  }

  //5. ACTUALIZAR UN SOCIO
  actualizarSocio(id: string, datos: any): Observable<any> {
    return this.http.put(`${this.apiUrl}/socios/actualizar/${id}`, datos).pipe(
      catchError(this.handleError<any>('actualizarSocio'))
    );
  }

  //6. ELIMINAR UN SOCIO
  eliminarSocio(id: string): Observable<any> {
    return this.http.delete<any>(`${this.apiUrl}/socios/eliminar/${id}`);
  }

  //******************** SERVICIO PARA APORTES */
  // 1. OBTENER APORTES
  getAportes(): Observable<Aporte[]> {
    // Ruta al endpoint que devuelve los aportes
    return this.http.get<Aporte[]>(`${this.apiUrl}/aportes`).pipe(
      catchError(this.handleError<Aporte[]>('getAportes', [] as Aporte[]))
    );
  }

  getAportesFiltrados(idSocio?: string, fechaInicio?: string, fechaFin?: string): Observable<any[]> {
    let url = `${this.apiUrl}/aportes/filtrar?`;

    if (idSocio) url += `idSocio=${idSocio}&`;
    if (fechaInicio) url += `fechaInicio=${fechaInicio}&`;
    if (fechaFin) url += `fechaFin=${fechaFin}`;

    return this.http.get<any[]>(url);
  }

  // 2. OBTENER SIGUIENTE ID PARA NUEVO APORTE
  getSiguienteIdAporte(): Observable<{ siguienteId: number }> {
    return this.http.get<{ siguienteId: number }>(`${this.apiUrl}/aportes/siguiente-id`).pipe(
      catchError((error) => {
        console.error('Error al obtener el siguiente ID:', error);
        return of({ siguienteId: 1 });
      })
    );
  }

  // 3. REGISTRAR UN NUEVO APORTE
  registrarAporte(nuevoAporte: Aporte): Observable<Aporte> {
    return this.http.post<Aporte>(`${this.apiUrl}/aportes/guardar`, nuevoAporte).pipe(
      tap((res: Aporte) => console.log('Aporte guardado con éxito:', res)),
      catchError((error) => {
        // Este error suele ser el 500 si idsocio es un objeto o idaporte ya existe
        console.error('Error en registrarAporte:', error);
        throw error;
      })
    );
  }

  // 4. OBTENER UN APORTE POR ID
  getAporteById(id: number): Observable<any> {
    return this.http.get<any>(`${this.apiUrl}/aportes/${id}`).pipe(
      catchError(this.handleError<any>('getAporteById'))
    );
  }

  // 5. ACTUALIZAR UN APORTE
  actualizarAporte(id: number, datos: any): Observable<any> {
    return this.http.put<any>(`${this.apiUrl}/aportes/actualizar/${id}`, datos).pipe(
      catchError(this.handleError<any>('actualizarAporte'))
    );
  }

  // 6. ELIMINAR UN APORTE
  eliminarAporte(id: number): Observable<any> {
    return this.http.delete<any>(`${this.apiUrl}/aportes/eliminar/${id}`).pipe(
      catchError(this.handleError<any>('eliminarAporte'))
    );
  }

  //******************** SERVICIO PARA PRESTAMOS */
  // 1. OBTENER PRESTAMOS
  getPrestamos(): Observable<Prestamos[]> {
    // Ruta al endpoint que devuelve los aportes
    return this.http.get<Prestamos[]>(`${this.apiUrl}/prestamos`).pipe(
      catchError(this.handleError<Prestamos[]>('getPrestamos', [] as Prestamos[]))
    );
  }

  getPrestamosFiltrados(idSocio?: number, fechaInicio?: string, fechaFin?: string): Observable<any[]> {
    let url = `${this.apiUrl}/prestamos/filtrar?`;

    if (idSocio) url += `idSocio=${idSocio}&`;
    if (fechaInicio) url += `fechaInicio=${fechaInicio}&`;
    if (fechaFin) url += `fechaFin=${fechaFin}`;

    return this.http.get<any[]>(url);
  }


  //2. ORBTENER SIGUIENTE ID PARA NUEVO PRESTAMO
  // getSiguienteIdPrestamo(tipo: string): Observable<{ siguienteId: string }> {
  //   return this.http.get<{ siguienteId: string }>(`${this.apiUrl}/prestamos/siguiente-id/${tipo}`);
  // }
  getSiguienteIdPrestamo(): Observable<{ siguienteId: number }> {
    return this.http.get<{ siguienteId: number }>(`${this.apiUrl}/prestamos/siguiente-id`);
  }

  // 3. OBTENER PRESTAMOS PENDIENTES PARA EL AUTOCOMPLETE DE PAGOS
  getPrestamosPendientesLista(): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/prestamos/pendientes`).pipe(
      map(data => data || []), // Si llega null, devuelve un array vacío para no romper el .filter()
      catchError(err => {
        console.error('Error en el servicio de préstamos:', err);
        // Retornamos un array vacío para que el componente no explote al intentar filtrar
        return throwError(() => new Error('No se pudo cargar la lista de préstamos.'));
      })
    );
  }

  // 3.1 OBTENER LOS DETALLES DE UN PRESTAMO POR ID (para mostrar datos adicionales al seleccionar en el autocomplete)
  getDetallesPrestamo(id: string): Observable<any> {
    return this.http.get(`${this.apiUrl}/prestamos/detalles/${id}`);
  }

  // 3.1.2 OBTENER PRESTAMOS PENDIENTES POR SOCIO
  getPrestamosPendientePorSocio(idSocio?: any): Observable<any[]> {
    // Si idSocio existe, lo añade a la URL, si no, llama al endpoint base
    const url = idSocio
      ? `${this.apiUrl}/prestamos/pendientes/socio/${idSocio}`//
      : `${this.apiUrl}/prestamos/pendientes/socio`;
    return this.http.get<any[]>(url);
  }

  // 3.1.3 OBTENER NUMERO DE PRESTAMOS Y PAGOS
  getResumenGlobal(): Observable<any> {
    return this.http.get<any>(`${this.apiUrl}/prestamos/resumen-global`);
  }
  getResumenPorSocio(idSocio?: string): Observable<any> {
    const url = idSocio
      ? `${this.apiUrl}/prestamos/resumen-socio/${idSocio}`
      : `${this.apiUrl}/prestamos/resumen-socio`;
    return this.http.get<any>(url);
  }


  // 3.2 OBTENER PRESTAMOS POR SOCIO
  getPrestamosBySocio(idsocio: string): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/prestamos/socio/${idsocio}`);
  }

  // 4. REGISTRAR UN NUEVO PRESTAMO
  registrarPrestamo(nuevoPrestamo: Prestamos): Observable<Prestamos> {
    // return this.http.post<Prestamos>(`${this.apiUrl}/prestamos`, nuevoPrestamo).pipe(
    return this.http.post<Prestamos>(`${this.apiUrl}/prestamos/guardar`, nuevoPrestamo).pipe(
      tap((res: Prestamos) => console.log('Prestamo guardado con éxito:', res)),
      catchError((error) => {
        // Este error suele ser el 500 si idsocio es un objeto o idaporte ya existe
        console.error('Error en registrarPrestamo:', error);
        throw error;
      })
    );
  }

  // 5. OBTENER UN PRESTAMO POR ID
  getPrestamoById(id: string): Observable<any> {
    return this.http.get<any>(`${this.apiUrl}/prestamos/${id}`).pipe(
      catchError(this.handleError<any>('getPrestamoById'))
    );
  }

  // 6. ACTUALIZAR UN PRESTAMO
  actualizarPrestamo(id: number, datos: any): Observable<any> {
    return this.http.put<any>(`${this.apiUrl}/prestamos/actualizar/${id}`, datos).pipe(
      catchError(this.handleError<any>('actualizarPrestamo'))
    );
  }

  // 7. ELIMINAR UN PRESTAMO
  eliminarPrestamo(id: any): Observable<any> {
    return this.http.delete<any>(`${this.apiUrl}/prestamos/eliminar/${id}`);
  }

  //******************** SERVICIO PARA PAGOS */
  // 1. OBTENER PAGOS
  getPagos(): Observable<Pagos[]> {
    // Ruta al endpoint que devuelve los aportes
    return this.http.get<Pagos[]>(`${this.apiUrl}/pagos`).pipe(
      catchError(this.handleError<Pagos[]>('getPagos', [] as Pagos[]))
    );
  }

  getPagosFiltrados(idSocio?: number, fechaInicio?: string, fechaFin?: string): Observable<any[]> {
    let url = `${this.apiUrl}/pagos/filtrar?`;

    if (idSocio) url += `idSocio=${idSocio}&`;
    if (fechaInicio) url += `fechaInicio=${fechaInicio}&`;
    if (fechaFin) url += `fechaFin=${fechaFin}`;

    return this.http.get<any[]>(url);
  }

  //2. ORBTENER SIGUIENTE ID PARA NUEVO PAGO
  // getSiguienteIdPagos(tipo: string): Observable<{ siguienteId: string }> {
  //   return this.http.get<{ siguienteId: string }>(`${this.apiUrl}/pagos/siguiente-id/${tipo}`);
  // }
  getSiguienteIdPagos(): Observable<{ siguienteId: number }> {
    return this.http.get<{ siguienteId: number }>(`${this.apiUrl}/pagos/siguiente-id`);
  }

  // 3. REGISTRAR UN NUEVO PAGO
  registrarPagos(nuevoPago: Pagos): Observable<Pagos> {
    return this.http.post<Pagos>(`${this.apiUrl}/pagos/guardar`, nuevoPago).pipe(
      tap((res: Pagos) => console.log('Pago guardado con éxito:', res)),
      catchError((error) => {
        // Este error suele ser el 500 si idsocio es un objeto o idaporte ya existe
        console.error('Error en registrarPago:', error);
        throw error;
      })
    );
  }

  // 4. OBTENER UN PAGO POR ID
  getPagosById(id: string): Observable<any> {
    return this.http.get<any>(`${this.apiUrl}/pagos/${id}`).pipe(
      catchError(this.handleError<any>('getPagosById'))
    );
  }

  // 4.1 OBTENER PAGOS POR SOCIO
  getPagosBySocio(idsocio: string): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/pagos/socio/${idsocio}`);
  }

  // 5. ACTUALIZAR UN PAGO
  actualizarPago(id: string, pago: any): Observable<any> {
    return this.http.put(`${this.apiUrl}/pagos/actualizar/${id}`, pago).pipe(
      catchError(this.handleError<any>('actualizarPago'))
    );
  }

  // 6. ELIMINAR UN PAGO
  // eliminarPagos(id: any): Observable<any> {
  //   return this.http.delete<any>(`${this.apiUrl}/pagos/${id}`).pipe(
  //     catchError(this.handleError<any>('eliminarPagos'))
  //   );
  // }
  eliminarPagos(id: any): Observable<any> {
    return this.http.delete<any>(`${this.apiUrl}/pagos/eliminar/${id}`).pipe(
      catchError(this.handleError<any>('eliminarPagos'))
    );
  }

  //******************** SERVICIO PARA CAJA */
  // 1. OBTENER APORTES
  getCaja(): Observable<Caja[]> {
    // Ruta al endpoint que devuelve los aportes
    return this.http.get<Caja[]>(`${this.apiUrl}/caja`).pipe(
      catchError(this.handleError<Caja[]>('getCaja', [] as Caja[]))
    );
  }

  // 2. OBTENER SIGUIENTE ID PARA NUEVO APORTE
  getSiguienteIdCaja(): Observable<{ siguienteId: number }> {
    return this.http.get<{ siguienteId: number }>(`${this.apiUrl}/caja/siguiente-id`).pipe(
      catchError((error) => {
        console.error('Error al obtener el siguiente ID:', error);
        return of({ siguienteId: 1 });
      })
    );
  }

  // 3. REGISTRAR UN NUEVA CAJA
  registrarCaja(nuevoCaja: Caja): Observable<Caja> {
    return this.http.post<Caja>(`${this.apiUrl}/caja/guardar`, nuevoCaja)
    // .pipe(
    //   tap((res: Caja) => console.log('Caja guardada con éxito:', res)),
    //   catchError((error) => {
    //     // Este error suele ser el 500 si idsocio es un objeto o idaporte ya existe
    //     console.error('Error en registrarCaja:', error);
    //     throw error;
    //   })
    // );
  }

  // 4. OBTENER UNA CAJA POR ID
  getCajaById(id: number): Observable<any> {
    return this.http.get<any>(`${this.apiUrl}/caja/${id}`).pipe(
      catchError(this.handleError<any>('getCajaById'))
    );
  }

  // 5. ACTUALIZAR UNA CAJA
  actualizarCaja(id: number, datos: any): Observable<any> {
    return this.http.put<any>(`${this.apiUrl}/caja/actualizar/${id}`, datos).pipe(
      catchError(this.handleError<any>('actualizarCaja'))
    );
  }

  // 6. ELIMINAR UNA CAJA
  eliminarCaja(id: number): Observable<any> {
    return this.http.delete<any>(`${this.apiUrl}/caja/eliminar/${id}`).pipe(
      catchError(this.handleError<any>('eliminarCaja'))
    );
  }

  //******************** SERVICIO PARA INFORMES */
  getResumenAportes(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/aportes`, { params });
  }

  getResumenPrestmos(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/prestamos`, { params });
  }

  getResumenAyudas(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/ayudas`, { params });
  }

  getResumenPrestmosMensual(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/prestamos/mensual`, { params });
  }

  getResumenIntersPrestmos(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/interesprestamos`, { params });
  }

  getResumenInteresPrestmosMensual(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/interesprestamos/mensual`, { params });
  }

  getResumenDineroPagado(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/dineropagado`, { params });
  }

  getResumenDineroPagadoMensual(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/dineropagado/mensual`, { params });
  }

  getResumenInteresPagado(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/interespagado`, { params });
  }

  getResumenInteresPagadoMensual(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/interespagado/mensual`, { params });
  }

  getResumenIngresosVarios(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/ingresosvarios`, { params });
  }

  getResumenEgresosVarios(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/gastosvarios`, { params });
  }

  getResumenInteresAnulado(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/interesanulado`, { params });
  }

  getResumenAporteIndividual(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/aporteindividual`, { params });
  }

  getResumenDineroCaja(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/dinerocaja`, { params });
  }

  getResumenAporteTotal(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/aportestotales`, { params });
  }

  getResumen1Aporte(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/aportesmensual`, { params });
  }

  getResumen2Prestamo(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/prestamosmensual`, { params });
  }

  getResumen3Efectivo(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/dineropagadomensualefectivo`, { params });
  }

  getResumen4Represtamo(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/dineropagadomensualreprestamo`, { params });
  }

  getResumen5InteresPrestado(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/interesprestamosmensual`, { params });
  }

  getResumen6InteresPagado(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/interespagadomensual`, { params });
  }

  getResumenCajaAnterior(inicio: string, fin: string): Observable<any> {
    const params = new HttpParams()
      .set('inicio', inicio)
      .set('fin', fin);

    return this.http.get(`${this.apiUrl}/reportes/caja-anterior`, { params });
  }


  // MÉTODO GENÉRICO DE MANEJO DE ERRORES
  private handleError<T>(operation = 'operation', result?: T) {
    return (error: any): Observable<T> => {
      console.error(`[${operation}] falló: ${error.message}`);
      return of(result as T);
    };
  }

}