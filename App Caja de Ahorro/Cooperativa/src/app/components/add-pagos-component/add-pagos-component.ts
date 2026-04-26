import { Component, OnInit } from '@angular/core';
import { AbstractControl, FormBuilder, FormControl, FormGroup, FormsModule, ReactiveFormsModule, ValidationErrors, Validators } from '@angular/forms';
import { Pagos } from '../../models/Pagos';
import { ActivatedRoute, Router } from '@angular/router';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatAutocompleteModule, MatAutocompleteSelectedEvent } from '@angular/material/autocomplete';
import { MatButtonModule } from '@angular/material/button';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { Socio } from '../../models/Socio';
import { map, Observable, startWith } from 'rxjs';
import { SociosService } from '../../services/socios';
import { MatSnackBar } from '@angular/material/snack-bar';
import { CommonModule } from '@angular/common';
import jsPDF from 'jspdf';
import autoTable from 'jspdf-autotable';

@Component({
  selector: 'app-add-pagos-component',
  imports: [CommonModule, ReactiveFormsModule, MatFormFieldModule, MatInputModule, MatAutocompleteModule, MatButtonModule, MatDatepickerModule, MatNativeDateModule],
  templateUrl: './add-pagos-component.html',
  styleUrl: './add-pagos-component.css',
})
export class AddPagosComponent implements OnInit {

  addPagosForm!: FormGroup;
  prestamosPendientes: any[] = [];
  prestamosFiltrados!: Observable<any[]>;

  sociosDisponibles: any[] = [];
  filteredSocios!: Observable<any[]>;
  sociosLista: any[] = []; // <-- DEBES AÑADIR ESTA LÍNEA
  listaPrestamos: any[] = [];
  datosPrestamoSeleccionado: any = {};

  esEdicion: boolean = false;
  esEdicionEgreso: boolean = false;
  tipoSeleccionado: 'PAGO' | 'INGRESO' | 'EGRESO' | null = null;
  tipoSeleccionado2: 'FAMILIAR' | 'FRANCESA' | 'ALEMANA' | 'AYUDA' | null = null;

  constructor(
    private fb: FormBuilder,
    private sociosService: SociosService,
    private router: Router,
    private route: ActivatedRoute,
    private snackBar: MatSnackBar
  ) {
    // 1. INICIALIZAR EL FORMULARIO PRIMERO (Evita errores de null en observables)
    this.initForm();
  }

  ngOnInit() {
    // 2. CARGAR DATOS INICIALES (Socios y Préstamos de la DB)
    this.cargarDatosBase();

    // 3. CONFIGURAR BUSCADORES (Después de cargar la data y el form)
    this.setupAutocompletes();

    this.setupSocioFilter(); // <-- AGREGA ESTA LÍNEA AQUÍ

    // 4. VERIFICAR SI ES EDICIÓN
    const id = this.route.snapshot.paramMap.get('id');
    if (id) {
      this.esEdicion = true;
      this.cargarDatosParaEdicion(id);
    } else {
      this.obtenerNuevoId(); // ID sugerido inicial
    }

    // Escucha cambios en el campo de interés para actualizar el comentario en tiempo real
    this.addPagosForm.get('interes')?.valueChanges.subscribe(nuevoValor => {
      this.actualizarComentario(nuevoValor);
    });
  }


  private initForm() {

    this.addPagosForm = this.fb.group({
      idPagos: ['', [Validators.required]],
      tipoPago: ['', [Validators.required]],
      idSocio: ['', [Validators.required]],
      nombreSocio: [{ value: '', disabled: false }],
      // Eliminamos el patrón estricto para permitir '0' o IDs numéricos
      idPrestamos: ['', [Validators.required]],
      amortizacion: [0, [Validators.required, Validators.pattern(/^-?\d+(\.\d*)?$/)]],
      fechaAmortizacion: [new Date(), Validators.required],
      // Cambiamos el patrón de numAmortizacion para que acepte letras (ING/EGR) o números
      numAmortizacion: ['', [Validators.required, Validators.pattern(/^[a-zA-Z0-9]+$/)]],
      interes: [0, [Validators.required, Validators.pattern(/^-?\d+(\.\d*)?$/)]],
      fechaInteres: [new Date(), Validators.required],
      numInteres: ['', [Validators.required, Validators.pattern(/^[a-zA-Z0-9]+$/)]],
      comentario: ['S/N', [Validators.required]]
    }, {
      // Aplicamos el validador personalizado al grupo completo
      validators: this.validarMontosNoAmbosCero
    });
  }

  private validarMontosNoAmbosCero(group: AbstractControl): ValidationErrors | null {
    const amortizacion = parseFloat(group.get('amortizacion')?.value || 0);
    const interes = parseFloat(group.get('interes')?.value || 0);

    // Si ambos son 0, retorna un error; de lo contrario, es válido
    if (amortizacion === 0 && interes === 0) {
      return { ambosCero: true };
    }
    return null;
  }

  // --- PASO 2: Carga de Datos desde API ---
  private cargarDatosBase() {
    // Cargar Socios
    // this.sociosService.getAllSocios().subscribe({
    //   next: (data) => this.sociosDisponibles = data,
    //   error: (err) => console.error('Error socios:', err)
    // });

    // Cargar Préstamos Pendientes (La lista que corregimos en SQL)
    this.sociosService.getPrestamosPendientesLista().subscribe({
      next: (data) => {
        this.prestamosPendientes = data;
        // Forzamos un trigger inicial para que la lista aparezca al hacer clic
        this.addPagosForm.get('idPrestamos')?.updateValueAndValidity();
        // ESTA LÍNEA MOSTRARÁ EL ARREGLO EN LA CONSOLA
        console.log('Lista de Préstamos Recibida:', data);
      },
      error: (err) => console.error('Error préstamos:', err)
    });
  }

  // --- PASO 3: Configuración de Buscadores (Observables) ---
  private setupAutocompletes() {
    // Filtro para Préstamos
    this.prestamosFiltrados = this.addPagosForm.get('idPrestamos')!.valueChanges.pipe(
      startWith(''),
      map(value => this._filterPrestamos(value))
    );

    // Filtro para Socios
    this.filteredSocios = this.addPagosForm.get('idSocio')!.valueChanges.pipe(
      startWith(''),
      map(value => this._filterSocios(value))
    );
  }

  private _filterPrestamos(value: any): any[] {
    const filterValue = typeof value === 'string' ? value.toLowerCase() : '';
    return this.prestamosPendientes.filter(p =>
      p.idprestamos.toString().toLowerCase().includes(filterValue) ||
      p.nombresocio.toLowerCase().includes(filterValue)
    );
  }


  private _filterSocios(value: any): any[] {
    // Validamos que el valor no sea nulo antes de procesar
    const filterValue = (typeof value === 'string' ? value : (value?.nombresocio || '')).toLowerCase();
    return this.sociosLista.filter(s =>
      (s.idsocio?.toString() || '').includes(filterValue) ||
      (s.nombresocio?.toLowerCase() || '').includes(filterValue)
    );
  }

  mostrarTextoPrestamo(prestamo: any): string {
    if (!prestamo) return '';
    // Si es el objeto que viene de la DB, mostramos "(ID) Nombre"
    return prestamo.idprestamos ? `(${prestamo.idprestamos}) ${prestamo.nombresocio}` : prestamo;
  }


  // onPrestamoSeleccionado(event: any) {
  //   const p = event.option.value;
  //   if (!p) return;

  //   this.addPagosForm.patchValue({
  //     idSocio: p.idsocio,
  //     tipoPago: this.tipoSeleccionado === 'INGRESO' ? 'INGRESO' : this.tipoSeleccionado === 'EGRESO' ? 'EGRESO' : 'PAGO',
  //     nombreSocio: p.nombresocio,
  //     // Solo autocompletamos montos si no es Egreso manual
  //     amortizacion: this.tipoSeleccionado !== 'INGRESO' ? p.proximaAmortizacion : 0,
  //     interes: this.tipoSeleccionado !== 'INGRESO' ? p.proximoInteres : 0,
  //     numAmortizacion: p.num_amortizacion || 1,
  //     numInteres: p.num_interes || 1,
  //     fechaAmortizacion: new Date(),
  //     fechaInteres: new Date()
  //   });

  //   // 2. VALIDACIÓN DE PREFIJOS:
  //   // Si es PAGO, usamos los números de cuota normales (1, 2, 3...)
  //   if (this.tipoSeleccionado === 'PAGO') {
  //     this.addPagosForm.patchValue({
  //       numAmortizacion: p.num_amortizacion || 1,
  //       numInteres: p.num_interes || 1
  //     });
  //   }
  //   // Si es INGRESO o EGRESO, forzamos el prefijo si el monto es 0 o vacío
  //   else {
  //     const prefijo = this.tipoSeleccionado === 'INGRESO' ? 'IGR' : 'GST';

  //     this.addPagosForm.patchValue({
  //       numAmortizacion: prefijo,
  //       numInteres: prefijo
  //     });

  //     // Dentro de la lógica donde detectas el cambio a EGRESO
  //     if (this.tipoSeleccionado === 'EGRESO') {
  //       this.addPagosForm.patchValue({
  //         amortizacion: 0,
  //       });
  //     }
  //   }

  //   // --- NUEVA LÓGICA: LLAMADA AL SERVICIO PARA LOS 8 INPUTS ---
  //   // ERROR AQUÍ: Probablemente estás pasando 'p' o un valor mal formado
  //   // CORRECCIÓN: Pasa solo el campo que contiene el ID (ej: p.idprestamos)
  //   const idParaConsulta = p.idprestamos;

  //   this.sociosService.getDetallesPrestamo(idParaConsulta).subscribe({
  //     next: (res) => {
  //       this.datosPrestamoSeleccionado = res;
  //     },
  //     error: (err) => console.error('Error al capturar detalles', err)
  //   });

  //   // Cambia esta parte al final de onPrestamoSeleccionado
  //   const interesActual = this.addPagosForm.get('interes')?.value; //
  //   this.actualizarComentario(interesActual); // Pasamos el interés

  // }

  actualizarComentario(valorInteres: any) {
    const tipo = this.tipoSeleccionado;
    const interes = parseFloat(valorInteres); // Convertimos a número para comparar

    // Si es PAGO y el INTERÉS es 0, es un represtamo
    if (tipo === 'PAGO' && interes === 0) {
      this.addPagosForm.patchValue({
        comentario: 'PAGO CON REPRESTAMO DE LA LETRA #'
      }, { emitEvent: false }); //
    }
    // Si es PAGO pero el INTERÉS es mayor a 0, es un pago normal
    else if (tipo === 'PAGO' && interes > 0) {
      this.addPagosForm.patchValue({
        comentario: 'PAGO DE LA LETRA #'
      }, { emitEvent: false }); //
    }
  }

  onPrestamoSeleccionado(event: any) {
    const p = event.option.value;
    if (!p) return;

    // 1. Cargamos datos básicos de identificación inmediatamente
    this.addPagosForm.patchValue({
      idSocio: p.idsocio,
      tipoPago: this.tipoSeleccionado === 'INGRESO' ? 'INGRESO' : this.tipoSeleccionado === 'EGRESO' ? 'EGRESO' : 'PAGO',
      nombreSocio: p.nombresocio,
      fechaAmortizacion: new Date(),
      fechaInteres: new Date()
    });

    // 2. Llamada al servicio para obtener los cálculos REALES del backend
    const idParaConsulta = p.idprestamos;

    this.sociosService.getDetallesPrestamo(idParaConsulta).subscribe({
      next: (res) => {
        // Guardamos los datos para los 8 cuadros informativos de abajo
        this.datosPrestamoSeleccionado = res;

        // --- CRUCIAL: Aquí es donde los valores cambian según la letra ---
        if (this.tipoSeleccionado === 'PAGO') {
          this.addPagosForm.patchValue({
            // Usamos los nombres exactos que devuelve tu nuevo backend
            amortizacion: res.proximaAmortizacion,
            interes: res.proximoInteres,
            numAmortizacion: res.numLetra, // Ahora será '2' si ya pagó la '1'
            numInteres: res.numLetra
          });

          // Actualizamos el comentario con el interés real obtenido
          this.actualizarComentario(res.proximoInteres);
        }
        else {
          // Lógica para INGRESO/EGRESO (IG/GST)
          const prefijo = this.tipoSeleccionado === 'INGRESO' ? 'IGR' : 'GST';
          this.addPagosForm.patchValue({
            amortizacion: this.tipoSeleccionado === 'EGRESO' ? 0 : 0, // Ajustar según necesidad
            interes: this.tipoSeleccionado === 'INGRESO' ? 0 : res.proximoInteres, // Ajustar según necesidad
            numAmortizacion: prefijo,
            numInteres: prefijo
          });
        }
      },
      error: (err) => console.error('Error al capturar detalles', err)
    });
  }


  onSocioSelected(event: any) {
    const socio = event.option.value;
    if (socio) {
      this.addPagosForm.patchValue({
        // Asegúrate de que coincida con la propiedad de tu consola: idsocio
        idSocio: socio.idsocio,
        nombreSocio: socio.nombresocio
      });
    }
  }

  // --- PASO 5: Auxiliares de UI ---
  // displayPrestamoFn(p: any): string {
  //   return p ? `(${p.idprestamos}) ${p.nombresocio || ''}` : '';
  // }

  // Esta función ahora es el "motor" que refresca los datos
  refrescarDatosPrestamo(idPrestamo: any) {
    if (!idPrestamo) return;

    this.sociosService.getDetallesPrestamo(idPrestamo).subscribe({
      next: (res) => {
        this.datosPrestamoSeleccionado = res;

        if (this.tipoSeleccionado === 'PAGO') {
          this.addPagosForm.patchValue({
            amortizacion: res.proximaAmortizacion,
            interes: res.proximoInteres,
            numAmortizacion: res.numLetra, // <--- Aquí llegará el 2, 3, etc.
            numInteres: res.numLetra,
            comentario: `PAGO DE LA LETRA #${res.numLetra}` // Opcional: limpiar comentario
          });
          this.actualizarComentario(res.proximoInteres);
        }
      },
      error: (err) => console.error('Error al refrescar detalles', err)
    });
  }

  displayPrestamoFn(prestamo: any): string {
    if (!prestamo) return '';
    // Si es un objeto, muestra el ID y nombre; si es solo un ID, muestra el ID
    return prestamo.idprestamos ? `(${prestamo.idprestamos}) ${prestamo.nombresocio || ''}` : prestamo;
  }


  displaySocioFn(socio: any): string {
    if (!socio) return '';
    // Si es el objeto completo seleccionado de la lista
    if (typeof socio === 'object' && socio.idsocio) {
      return `(${socio.idsocio}) ${socio.nombresocio}`;
    }
    // Si es solo el ID (cuando parchas el valor desde el préstamo)
    return socio;
  }

  // seleccionarTipo(tipo: 'PAGO' | 'INGRESO' | 'EGRESO') {
  //   this.tipoSeleccionado = tipo;
  //   this.obtenerNuevoId(tipo);
  // }

  seleccionarTipo(tipo: 'PAGO' | 'INGRESO' | 'EGRESO') {
    this.tipoSeleccionado = tipo;
    // this.sociosLista = []; // <-- LIMPIA LA LISTA SIEMPRE AL INICIO
    this.limpiarCampos();
    this.obtenerNuevoId(); // Esto ya llena idPagos (Numero de Pago)

    const idPrestamosCtrl = this.addPagosForm.get('idPrestamos');
    const idSocioCtrl = this.addPagosForm.get('idSocio');
    const numAmortCtrl = this.addPagosForm.get('numAmortizacion');
    const numIntCtrl = this.addPagosForm.get('numInteres');
    const comentarioCtrl = this.addPagosForm.get('comentario');
    const tipoPagoCtrl = this.addPagosForm.get('tipoPago');

    const prefijo = tipo === 'INGRESO' ? 'IGR' : (tipo === 'EGRESO' ? 'GST' : '');

    if (tipo !== 'PAGO') {
      this.addPagosForm.patchValue({
        numAmortizacion: prefijo,
        numInteres: prefijo
      });
    }

    switch (tipo) {
      case 'PAGO':
        idSocioCtrl?.disable();
        idPrestamosCtrl?.enable();
        // En PAGO suelen ser números secuenciales (ej. 1, 2, 3)
        comentarioCtrl?.setValue('PAGO DE LA LETRA #');
        numAmortCtrl?.setValue('');
        numIntCtrl?.setValue('');
        this.sociosService.getSociosParaPagos().subscribe(data => this.sociosLista = data);
        break;

      case 'INGRESO':
        idPrestamosCtrl?.disable();
        idSocioCtrl?.enable();
        tipoPagoCtrl?.setValue('INGRESO'); // Asegura que el tipo se guarde como INGRESO
        // Asignamos el prefijo ING
        comentarioCtrl?.setValue('');
        numAmortCtrl?.setValue('IGR');
        numIntCtrl?.setValue('IGR');
        this.sociosService.getSociosParaIngresos().subscribe(data => this.sociosLista = data);
        break;

      case 'EGRESO':
        this.esEdicionEgreso = true;
        idPrestamosCtrl?.enable();
        idSocioCtrl?.enable();
        // Asignamos el prefijo EGR
        comentarioCtrl?.setValue('ANULACION DEL INTERES POR REEPRESTAMO');
        numAmortCtrl?.setValue('GST');
        numIntCtrl?.setValue('GST');
        this.sociosService.getSociosParaEgresos().subscribe(data => this.sociosLista = data);
        break;
    }
  }

  seleccionarTipo2(tipo: 'FAMILIAR' | 'FRANCESA' | 'ALEMANA' | 'AYUDA') {
    this.tipoSeleccionado2 = tipo;
    // Aquí podrías agregar lógica adicional si necesitas filtrar socios o préstamos según este tipo
  }


  setupFilters() {
    this.prestamosFiltrados = this.addPagosForm.get('idPrestamos')!.valueChanges.pipe(
      startWith(''),
      map(value => {
        const filterValue = typeof value === 'string' ? value.toLowerCase() : '';

        return this.prestamosPendientes.filter(p => {
          // DEFINICIÓN DE VARIABLES (Para evitar error 'Cannot find name')
          const socioIdStr = p.idsocio ? p.idsocio.toString() : '';
          const idPrestamoStr = p.idprestamos ? p.idprestamos.toString() : '';

          const idStr = p.idprestamos ? p.idprestamos.toString() : ''; // Agrega esta línea
          const coincideBusqueda = idPrestamoStr.toLowerCase().includes(filterValue) ||
            p.nombresocio.toLowerCase().includes(filterValue);

          // FILTRADO POR ID DE SOCIO (Independiente de la tabla préstamo)
          switch (this.tipoSeleccionado) {
            case 'PAGO':
              // Excluimos socios de sistema (01 y 02)
              return coincideBusqueda && socioIdStr !== '01' && socioIdStr !== '02';

            case 'INGRESO':
              // Permitimos socios reales + Socio 02 (GASTOS), excluimos 01
              return coincideBusqueda && socioIdStr !== '02';

            case 'EGRESO':
              // Permitimos socios reales + Socio 01 (INGRESOS), excluimos 02
              return coincideBusqueda && socioIdStr !== '01';

            default:
              return false;
          }
        });
      })
    );
  }

  setupSocioFilter() {
    this.filteredSocios = this.addPagosForm.get('idSocio')!.valueChanges.pipe(
      startWith(''),
      map(value => {
        // Si el valor es un objeto (socio seleccionado), usamos su nombre; si no, el string escrito
        const filterValue = typeof value === 'string' ? value.toLowerCase() : (value?.nombresocio?.toLowerCase() || '');

        // Filtramos sobre 'sociosLista', que ya viene filtrada desde el switch de seleccionarTipo
        return this.sociosLista.filter(s =>
          (s.idsocio?.toString() || '').toLowerCase().includes(filterValue) ||
          (s.nombresocio?.toLowerCase() || '').includes(filterValue)
        );
      })
    );
  }


  obtenerNuevoId() {
    this.sociosService.getSiguienteIdPagos().subscribe({
      next: (res) => this.addPagosForm.patchValue({ idPagos: res.siguienteId }),
      error: (err) => console.error('Error ID sugerido:', err)
    });
  }

  // Función de apoyo para formatear sin desfase
  private formatDate(date: Date): string {
    const d = new Date(date);
    let month = '' + (d.getMonth() + 1);
    let day = '' + d.getDate();
    const year = d.getFullYear();

    if (month.length < 2) month = '0' + month;
    if (day.length < 2) day = '0' + day;

    return [year, month, day].join('-');
  }

  // // --- PASO 6: Guardado de Datos ---
  // onSubmit() {
  //   const d = this.addPagosForm.getRawValue();

  //   // 1. Extraer la fecha del formulario
  //   const rawDate = this.addPagosForm.value.fechaAmortizacion;
  //   const rawDateInteres = this.addPagosForm.value.fechaInteres;

  //   // 2. Convertir a string "YYYY-MM-DD" ignorando horas
  //   const fechaLimpia = this.formatDate(rawDate);
  //   const fechaInteresLimpia = this.formatDate(rawDateInteres);
  //   // MAPEO DE DATOS: Asegúrate de usar minúsculas para que el backend 
  //   // no reciba NULL
  //   const finalData = {
  //     idpagos: d.idPagos,
  //     tipo: d.tipoPago,
  //     idsocio: d.idSocio?.idsocio || d.idSocio,
  //     nombresocio: d.nombreSocio,
  //     idprestamos: (this.tipoSeleccionado === 'INGRESO' || !d.idPrestamos) ? null : (d.idPrestamos?.idprestamos || d.idPrestamos),
  //     // CAMBIO AQUÍ: Extraer la amortización del objeto del préstamo si existe
  //     amortizacion: d.idPrestamos?.amortizacion || d.amortizacion || 0,
  //     fechaamortizacion: fechaLimpia,
  //     numamortizacion: d.numAmortizacion,
  //     interes: d.interes || 0,
  //     fechainteres: fechaInteresLimpia,
  //     numinteres: d.numInteres,
  //     comentario: d.comentario
  //   };

  //   if (this.esEdicion) {
  //     // Si estamos editando, usamos PUT para evitar el error de llave duplicada
  //     this.sociosService.actualizarPago(finalData.idpagos, finalData).subscribe({
  //       next: () => {
  //         alert('Actualizado con éxito');
  //         this.limpiarCampos(); // Limpiar el formulario después de actualizar
  //         this.router.navigate(['/listapagos']);
  //       },
  //       error: (err) => console.error('Error al actualizar:', err)
  //     });
  //   } else {
  //     // Si es nuevo, usamos POST
  //     this.sociosService.registrarPagos(finalData as any).subscribe({
  //       next: () => {
  //         // --- NUEVO: Generación de PDF al guardar con éxito ---
  //         this.generarPDFRecibo(finalData);
  //         alert('Guardado con éxito');
  //         this.router.navigate(['/pagos']);
  //         this.limpiarCampos(); // Limpiar el formulario después de guardar

  //         // 1. Obtenemos el ID del préstamo actual antes de limpiar nada
  //         const idPrestamoActual = d.idPrestamos?.idprestamos || d.idPrestamos;

  //         // 2. Si es un PAGO de préstamo, refrescamos los valores para la siguiente letra
  //         if (this.tipoSeleccionado === 'PAGO' && idPrestamoActual) {
  //           this.refrescarDatosPrestamo(idPrestamoActual);
  //           // this.limpiarCampos();
  //         } else {
  //           // Si es ingreso/egreso general, simplemente limpiamos
  //           this.limpiarCampos();
  //         }
  //       },
  //       // error: (err) => console.error('Error al guardar:', err)
  //     });
  //   }
  // }


  onSubmit() {
    const d = this.addPagosForm.getRawValue();
    const rawDate = this.addPagosForm.value.fechaAmortizacion;
    const rawDateInteres = this.addPagosForm.value.fechaInteres;

    const finalData = {
      idpagos: d.idPagos,
      tipo: d.tipoPago,
      idsocio: d.idSocio?.idsocio || d.idSocio,
      nombresocio: d.nombreSocio,
      idprestamos: (this.tipoSeleccionado === 'INGRESO' || !d.idPrestamos) ? null : (d.idPrestamos?.idprestamos || d.idPrestamos),
      amortizacion: d.idPrestamos?.amortizacion || d.amortizacion || 0,
      fechaamortizacion: this.formatDate(rawDate),
      numamortizacion: d.numAmortizacion,
      interes: d.interes || 0,
      fechainteres: this.formatDate(rawDateInteres),
      numinteres: d.numInteres,
      comentario: d.comentario
    };

    if (this.esEdicion) {
      this.sociosService.actualizarPago(finalData.idpagos, finalData).subscribe({
        next: () => {
          alert('Actualizado con éxito');
          this.limpiarCampos(); // <--- Limpieza total
          this.router.navigate(['/listapagos']);
        },
        error: (err) => console.error('Error al actualizar:', err)
      });
    } else {
      this.sociosService.registrarPagos(finalData as any).subscribe({
        next: () => {
          // Generamos el recibo antes de limpiar los datos de la memoria
          this.generarPDFRecibo(finalData);
          alert('Guardado con éxito');

          // IMPORTANTE: Limpiar el formulario y las variables de consulta
          this.limpiarCampos();

          // Si quieres que el usuario se quede en la misma página para seguir ingresando:
          this.router.navigate(['/pagos']);
        },
        error: (err) => alert('Error al guardar: verifique los datos o si el ID ya existe')
      });
    }
  }

  generarPDFRecibo(pago: any) {
    const doc = new jsPDF();
    const pageWidth = doc.internal.pageSize.getWidth();

    // Encabezado
    doc.setFontSize(18);
    doc.setTextColor(46, 204, 113); // Color verde de tu interfaz
    doc.text('Caja de Ahorro Familia Unida', pageWidth / 2, 20, { align: 'center' });

    doc.setFontSize(12);
    doc.setTextColor(0, 0, 0);
    doc.text(`Comprobante de ${pago.tipo} Electrónico`, pageWidth / 2, 28, { align: 'center' });
    doc.text(`N° Transacción: ${pago.idpagos}`, 15, 40);
    // // Convertimos la fecha a formato local corto (DD/MM/YYYY)
    // const fechaCorta = new Date(pago.fechaamortizacion).toLocaleDateString('es-ES');
    // doc.text(`Fecha: ${fechaCorta}`, pageWidth - 15, 40, { align: 'right' });
    // 1. Tomamos solo la parte de la fecha (YYYY-MM-DD) para evitar ruidos de horas
    const fechaString = pago.fechaamortizacion.split('T')[0];

    // 2. Separamos los componentes manualmente
    const [anio, mes, dia] = fechaString.split('-');

    // 3. Formateamos como DD/MM/YYYY
    const fechaCorta = `${dia}/${mes}/${anio}`;

    // 4. Insertamos en el PDF
    doc.text(`Fecha: ${fechaCorta}`, pageWidth - 15, 40, { align: 'right' });

    // Tabla de Detalles
    autoTable(doc, {
      startY: 45,
      head: [['Descripción', 'Detalle']],
      body: [
        ['Socio:', pago.nombresocio.toUpperCase()],
        ['ID Prestamo:', pago.idprestamos],
        ['Concepto:', pago.comentario || `Pago de cuota ${pago.numamortizacion || ''}`],
        ['Amortización:', `$ ${Number(pago.amortizacion).toFixed(2)}`],
        ['Interés:', `$ ${Number(pago.interes).toFixed(2)}`],
        ['TOTAL RECIBIDO:', `$ ${(Number(pago.amortizacion) + Number(pago.interes)).toFixed(2)}`],
      ],
      theme: 'grid',
      headStyles: { fillColor: [46, 204, 113], halign: 'center' },
      columnStyles: {
        0: { fontStyle: 'bold', cellWidth: 50 },
        1: { halign: 'left' }
      }
    });

    // --- SECCIÓN DE FIRMAS ---
    const finalY = (doc as any).lastAutoTable.finalY + 35; // Espacio después de la tabla

    // Línea y texto para el Socio
    doc.line(30, finalY, 80, finalY); // Línea izquierda
    doc.setFontSize(10);
    doc.text('Firma del Socio', 55, finalY + 5, { align: 'center' });
    // doc.text(`CI: ${pago.idsocio}`, 55, finalY + 10, { align: 'center' });

    // Línea y texto para el Tesorero/Caja
    doc.line(pageWidth - 80, finalY, pageWidth - 30, finalY); // Línea derecha
    doc.text('Recibido por (Caja)', pageWidth - 55, finalY + 5, { align: 'center' });
    doc.text('Familia Unida', pageWidth - 55, finalY + 10, { align: 'center' });

    // Pie de página
    doc.setFontSize(8);
    doc.setTextColor(150);
    doc.text('Este documento es un comprobante interno de la Caja de Ahorro Familia Unida.', pageWidth / 2, finalY + 30, { align: 'center' });

    doc.save(`Recibo_${pago.idpagos}_${pago.idsocio}.pdf`);
  }

  // cargarDatosParaEdicion(id: string) {
  //   this.sociosService.getPagosById(id).subscribe({
  //     next: (data) => {
  //       // 1. Buscamos el objeto prestamo en la lista cargada para que el autocomplete lo reconozca
  //       const prestamoSeleccionado = this.listaPrestamos.find(p => p.idprestamos == data.idprestamos);

  //       // Cuando recibes los datos del servidor para llenar el formulario:
  //       const fechaDeBaseDatos1 = data.fechaamortizacion;
  //       const fechaDeBaseDatos2 = data.fechainteres; // Viene como "2026-04-20" o "2026-04-20T00:00:00.000Z"

  //       // Forzamos a que cree la fecha sin desfase
  //       const dateParts1 = fechaDeBaseDatos1.split('T')[0].split('-');
  //       const fechaParaElForm1 = new Date(
  //         Number(dateParts1[0]),
  //         Number(dateParts1[1]) - 1,
  //         Number(dateParts1[2])
  //       );
  //       const dateParts2 = fechaDeBaseDatos2.split('T')[0].split('-');
  //       const fechaParaElForm2 = new Date(
  //         Number(dateParts2[0]),
  //         Number(dateParts2[1]) - 1,
  //         Number(dateParts2[2])
  //       );
  //       this.addPagosForm.patchValue({
  //         idPagos: data.idpagos,
  //         tipoPago: data.tipo,
  //         idSocio: data.idsocio,
  //         nombreSocio: data.nombresocio,
  //         // 2. Asignamos el objeto encontrado o el ID si no es un autocomplete de objetos
  //         idPrestamos: prestamoSeleccionado || data.idprestamos || '0',
  //         amortizacion: data.amortizacion,
  //         // fechaAmortizacion: new Date(data.fechaamortizacion),
  //         fechaAmortizacion: fechaParaElForm1, // Usamos la fecha corregida
  //         numAmortizacion: data.numamortizacion,
  //         interes: data.interes,
  //         // fechaInteres: new Date(data.fechainteres),
  //         fechaInteres: fechaParaElForm2, // Usamos la fecha corregida
  //         numInteres: data.numinteres,
  //         comentario: data.comentario
  //       });

  //       // 2. DESACTIVAR CAMPOS CLAVE para que no se puedan borrar o cambiar
  //       // this.addPagosForm.get('idPagos')?.disable();
  //       // this.addPagosForm.get('idSocio')?.disable();
  //       // this.addPagosForm.get('nombreSocio')?.disable();
  //       // this.addPagosForm.get('idPrestamos')?.disable();
  //       // this.addPagosForm.get('numAmortizacion')?.disable();
  //       // this.addPagosForm.get('numInteres')?.disable();

  //       // 3. Establecer modo edición para bloquear los botones de tipo (PAGO, INGRESO, EGRESO)
  //       this.esEdicion = true;

  //       // Manejo de tipos para la interfaz visual
  //       if (data.idpagos.includes('INGRESO')) this.tipoSeleccionado = 'INGRESO';
  //       else if (data.idpagos.includes('EGRESO')) this.tipoSeleccionado = 'EGRESO';
  //       else this.tipoSeleccionado = 'PAGO';
  //     }
  //   });
  // }

  cargarDatosParaEdicion(id: string) {
    this.sociosService.getPagosById(id).subscribe({
      next: (data) => {
        const prestamoSeleccionado = this.listaPrestamos.find(p => p.idprestamos == data.idprestamos);

        // 1. Procesamiento de fechas para evitar desfase (Ya lo tenías bien)
        const fechaParaElForm1 = this.corregirFecha(data.fechaamortizacion);
        const fechaParaElForm2 = this.corregirFecha(data.fechainteres);

        // 2. Llenar el formulario
        this.addPagosForm.patchValue({
          idPagos: data.idpagos,
          tipoPago: data.tipo,
          idSocio: data.idsocio,
          nombreSocio: data.nombresocio,
          idPrestamos: prestamoSeleccionado || data.idprestamos || '0',
          amortizacion: data.amortizacion,
          fechaAmortizacion: fechaParaElForm1,
          numAmortizacion: data.numamortizacion,
          interes: data.interes,
          fechaInteres: fechaParaElForm2,
          numInteres: data.numinteres,
          comentario: data.comentario
        });

        // --- CAMBIO CRUCIAL AQUÍ ---
        // 3. Si existe un ID de préstamo, llamamos al servicio para llenar los cuadros informativos
        if (data.idprestamos && data.idprestamos !== '0') {
          this.sociosService.getDetallesPrestamo(data.idprestamos).subscribe({
            next: (res) => {
              // Esto llenará los campos que ahora salen como 'undefined'
              this.datosPrestamoSeleccionado = res;
            },
            error: (err) => console.error('Error al cargar detalles informativos', err)
          });
        }

        this.esEdicion = true;
        if (String(data.tipo).includes('PAGO')) this.tipoSeleccionado = 'PAGO';
        else if (String(data.tipo).includes('INGRESO')) this.tipoSeleccionado = 'INGRESO';
        else if (String(data.tipo).includes('EGRESO')) this.tipoSeleccionado = 'EGRESO';
        else this.tipoSeleccionado = 'PAGO';
      }
    });
  }

  // Función auxiliar para no repetir código de fechas
  private corregirFecha(fechaStr: string): Date {
    const parts = fechaStr.split('T')[0].split('-');
    return new Date(Number(parts[0]), Number(parts[1]) - 1, Number(parts[2]));
  }

  // limpiarCampos() {
  //   // 1. Reiniciamos todo el formulario
  //   this.addPagosForm.reset();

  //   // 2. Volvemos a colocar la fecha actual (importante para que no queden vacíos)
  //   const fechaHoy = new Date();
  //   this.addPagosForm.patchValue({
  //     fechaAmortizacion: fechaHoy,
  //     fechaInteres: fechaHoy,
  //     comentario: 'S/N' // Opcional: reiniciar el comentario por defecto
  //   });

  //   // 3. Restauramos el ID sugerido según el tipo seleccionado
  //   if (this.tipoSeleccionado) {
  //     this.obtenerNuevoId();
  //   } else {
  //     this.obtenerNuevoId();
  //   }

  //   // 4. LIMPIAR LOS 8 DATOS ADICIONALES (La parte de abajo)
  //   this.datosPrestamoSeleccionado = {
  //     montoPrestado: null,
  //     fechaPrestamo: null,
  //     plazoPrestamo: null,
  //     cuotaPrestamo: null,
  //     letrasPagadas: null,
  //     letrasPendientes: null,
  //     amortizacionPendiente: null,
  //     interesPendiente: null
  //   };
  // }

  limpiarCampos() {
    // 1. Reiniciamos el formulario con valores iniciales seguros
    // Esto evita que campos como 'tipoPago' queden nulos y rompan la interfaz
    this.addPagosForm.reset({
      tipoPago: 'PAGO', // O el tipo que manejas por defecto
      fechaAmortizacion: new Date(),
      fechaInteres: new Date(),
      comentario: 'S/N',
      amortizacion: 0,
      interes: 0
    });

    // 2. IMPORTANTE: Limpiar el objeto de consulta para que los cuadros 
    // informativos de abajo se vacíen completamente
    this.datosPrestamoSeleccionado = null;

    // 3. Restaurar estados de control
    this.esEdicion = false;

    // 4. Generar el nuevo ID para el siguiente registro
    // Esto asegura que el campo 'Número de Pago' se actualice
    this.obtenerNuevoId();
  }

  irASocio(): void {
    this.router.navigate(['/socios']);
  }

  irListaPagos(): void {
    this.router.navigate(['/listapagos']);
  }

  lanzarAlerta() {
    this.snackBar.open('⚠️ Elige la forma de cancelación (PAGO, INGRESO o EGRESO) para habilitar el formulario', 'Cerrar', {
      duration: 4000,
      verticalPosition: 'top',
      panelClass: ['warning-snackbar']
    });
  }
}
