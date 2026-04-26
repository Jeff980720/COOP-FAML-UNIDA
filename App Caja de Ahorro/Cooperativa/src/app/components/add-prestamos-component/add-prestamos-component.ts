import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormBuilder, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatAutocompleteModule, MatAutocompleteSelectedEvent } from '@angular/material/autocomplete';
import { MatButtonModule } from '@angular/material/button';
import { MatNativeDateModule } from '@angular/material/core';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { Prestamos } from '../../models/Prestamos';
import { ActivatedRoute, Router } from '@angular/router';
import { map, Observable, startWith, Subject, takeUntil, throwError } from 'rxjs';
import { Socio } from '../../models/Socio';
import { SociosService } from '../../services/socios';
import { MatDividerModule } from '@angular/material/divider';
import { MatTableModule } from '@angular/material/table';
import { MatSnackBar } from '@angular/material/snack-bar';
import { jsPDF } from 'jspdf';
import autoTable from 'jspdf-autotable'; // Importación correcta para versiones modernas

// Define la estructura de cada fila de la tabla
interface AmortizacionRow {
  periodo: number;
  fechaPago: string;     // <-- AGREGA ESTA LÍNEA
  saldoInicial: number;
  cuota: number;
  interes: number;
  amortizacion: number;
  saldoFinal: number;
}

interface TotalesAmortizacion {
  interesTotal: number;
  amortizacionTotal: number;
  cuotaTotal: number;
}

@Component({
  selector: 'app-add-prestamos-component',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatAutocompleteModule,
    MatButtonModule,
    MatDatepickerModule,
    MatNativeDateModule,
    FormsModule,
    MatDividerModule,
    MatTableModule,
  ],
  templateUrl: './add-prestamos-component.html',
  styleUrl: './add-prestamos-component.css',
})

export class AddPrestamosComponent {
  addPrestamoForm: FormGroup;
  prestamo!: Prestamos
  sociosDisponibles: Socio[] = [];
  filteredSocios!: Observable<Socio[]>;
  private destroy$: Subject<void> = new Subject<void>();
  // En tu componente (dentro de la clase)
  tablaAmortizacion1: AmortizacionRow[] = [];
  tablaAmortizacion2: AmortizacionRow[] = [];
  tablaAmortizacion3: AmortizacionRow[] = [];
  displayedColumns: string[] = ['periodo', 'saldoInicial', 'cuota', 'interes', 'amortizacion', 'saldoFinal'];
  esEdicion: boolean = false;
  idEdicion: string | null = null;
  tipoSeleccionado: 'PRESTAMO' | 'AYUDA' | null = null;
  // tipoSeleccionado2: 'FAMILIAR' | 'FRANCESA' | 'ALEMANA' | null = null;
  // Por esto (agregando 'AYUDA'):
  tipoSeleccionado2: 'FAMILIAR' | 'FRANCESA' | 'ALEMANA' | 'AYUDA' | null = null;
  Number = Number; // Esto permite usar la función Number en el HTML
  totalesTabla1: TotalesAmortizacion = { interesTotal: 0, amortizacionTotal: 0, cuotaTotal: 0 };
  totalesTabla2: TotalesAmortizacion = { interesTotal: 0, amortizacionTotal: 0, cuotaTotal: 0 };
  totalesTabla3: TotalesAmortizacion = { interesTotal: 0, amortizacionTotal: 0, cuotaTotal: 0 };

  constructor(private router: Router, private fb: FormBuilder, private sociosService: SociosService, private route: ActivatedRoute, private snackBar: MatSnackBar) {
    this.addPrestamoForm = this.fb.group({
      idPrestamos: ['', Validators.required],
      tipoPrestamo: ['', Validators.required],
      idSocio: ['', Validators.required],
      nombreSocio: [{ value: '', disabled: false }], // Campo solo para mostrar el nombre, no editable
      fechaPrestamo: ['', Validators.required],
      montoPrestado: ['', [Validators.required,
      // REGLA: Decimal o Entero (positivo o negativo), NO CERO (0)
      // Patrón: /^-?(?!0\d*$)\d+(\.\d+)?$/
      Validators.pattern(/^-?(?!0\d*$)\d+(\.\d+)?$/)]
      ],
      plazoPrestamo: ['', [Validators.required,
      // REGLA: Solo números enteros (dígitos del 0 al 9)
      // El patrón /^-?\d+$/ permite un signo de menos opcional (-) seguido de uno o más dígitos.
      Validators.pattern(/^-?\d+$/)]
      ],
      interesPrestamo: ['', [Validators.required,
      // // El patrón /^[0-9]*$/ asegura que solo haya dígitos.
      // Validators.pattern(/^[0-9]*$/),
      // REGLA: Solo números enteros (dígitos del 0 al 9)
      // El patrón /^-?\d+$/ permite un signo de menos opcional (-) seguido de uno o más dígitos.
      Validators.pattern(/^-?\d+$/)]
      ],
      interesMensual: [''],
      interesTotal: [''],
      amortizacion: [''],
      cuota: [''],
      total: [''],
      comentario: ['', Validators.required],
    })
  }

  onSubmit() {
    if (this.addPrestamoForm.valid) {
      const formVal = this.addPrestamoForm.getRawValue();
      this.prestamo = this.addPrestamoForm.value;
      const nuevoPrestamo: Prestamos = {
        idprestamos: this.addPrestamoForm.get('idPrestamos')!.value,
        tipo: this.addPrestamoForm.get('tipoPrestamo')!.value,
        idsocio: this.addPrestamoForm.get('idSocio')!.value,
        nombresocio: this.addPrestamoForm.get('nombreSocio')!.value,
        fechaprestamo: this.addPrestamoForm.get('fechaPrestamo')!.value,
        montoprestado: this.addPrestamoForm.get('montoPrestado')!.value,
        plazoprestamo: this.addPrestamoForm.get('plazoPrestamo')!.value,
        interesprestamo: this.addPrestamoForm.get('interesPrestamo')!.value,
        interesmensual: this.addPrestamoForm.get('interesMensual')!.value,
        interestotal: this.addPrestamoForm.get('interesTotal')!.value,
        amortizacion: this.addPrestamoForm.get('amortizacion')!.value,
        cuota: this.addPrestamoForm.get('cuota')!.value,
        total: this.addPrestamoForm.get('total')!.value,
        comentario: this.addPrestamoForm.get('comentario')!.value,
      };
      console.log('Nuevo Pago agregado:', nuevoPrestamo);
    } else {
      console.log('Formulario de Prestamos no es válido');
      this.addPrestamoForm.markAllAsTouched();
    }
  }

  ngOnInit() {

    this.addPrestamoForm = this.fb.group({
      idPrestamos: ['', Validators.required],
      tipoPrestamo: ['', Validators.required],
      idSocio: ['', Validators.required],
      nombreSocio: [{ value: '', disabled: false }], // Campo solo para mostrar el nombre, no editable
      fechaPrestamo: [new Date(), Validators.required],
      montoPrestado: ['', [Validators.required, Validators.pattern(/^-?(?!0\d*$)\d+(\.\d+)?$/)]],
      plazoPrestamo: ['', [Validators.required, Validators.pattern(/^-?\d+$/)]],
      // interesPrestamo: ['', [Validators.required, Validators.pattern(/^-?\d+$/)]],
      interesPrestamo: ['', [Validators.required, Validators.pattern(/^-?(?!0\d*$)\d+(\.\d+)?$/)]],
      interesMensual: [''],
      interesTotal: [''],
      amortizacion: [''],
      cuota: [''],
      total: [''],
      comentario: ['', Validators.required],

      interesMensual2: [''],
      interesTotal2: [''],
      amortizacion2: [''],
      cuota2: [''],
      total2: [''],

      interesMensual3: [''],
      interesTotal3: [''],
      amortizacion3: [''],
      cuota3: [''],
      total3: [''],
    });

    // 2. CARGAR SOCIOS PRIMERO
    this.sociosService.getAllSocios().subscribe({
      next: (data) => {
        this.sociosDisponibles = data;
        this.configurarFiltroSocios();

        // 3. Solo después de tener la lista de socios, verificamos si es edición
        const id = this.route.snapshot.paramMap.get('id');
        if (id) {
          this.esEdicion = true;
          this.cargarDatosPrestamo(id); // Convertimos el ID de la URL a número
        } else {
          this.esEdicion = false;
          this.obtenerNuevoId();
        }
      },
      error: (err) => console.error('Error al cargar socios:', err)
    });

    // Llamar a la función que escuchará los cambios
    this.setupCalculationListener();
    // Carga la lista de socios
    this.sociosService.getAllSocios().subscribe(data => {//cambiado el getAllSocios
      this.sociosDisponibles = data;
    });

    // Configura el Autocompletado reactivo
    this.filteredSocios = this.addPrestamoForm.get('idSocio')!.valueChanges.pipe(
      startWith(''),
      // Mapea el valor a string (usa 'nombre' de tu modelo Socio)
      map(value => (typeof value === 'string' ? value : value?.nombre || '')),
      map(name => (name ? this._filter(name) : this.sociosDisponibles.slice()))
    );
  }

  configurarFiltroSocios() {
    this.filteredSocios = this.addPrestamoForm.get('idSocio')!.valueChanges.pipe(
      startWith(''),
      map(value => {
        const name = typeof value === 'string' ? value : value?.nombre;
        return name ? this._filter(name) : this.sociosDisponibles.slice();
      })
    );
  }

  // registrarPrestamo(tipo: string) {
  //   if (this.addPrestamoForm.invalid) return; //

  //   // 1. Obtenemos todos los valores (incluyendo los deshabilitados como numeroAporte)
  //   const formValues = this.addPrestamoForm.getRawValue();

  //   // // 2. EXTRAER EL ID: Si es un objeto (del autocomplete), tomamos .id; si no, el valor directo
  //   // // Usamos .id porque así está definido en tu modelo Socio
  //   // const valorSocio = formValues.idSocio;
  //   // const idSocioFinal = valorSocio && typeof valorSocio === 'object'
  //   //   ? valorSocio.idsocio
  //   //   : valorSocio;
  //   // UNIMOS LOS TIPOS: Ejemplo "PRESTAMO_FRANCESA"
  //   // Si es ayuda, se guarda solo como "AYUDA"
  //   // const tipoFinal = (tipoAmortizacion === 'AYUDA')
  //   //   ? 'AYUDA'
  //   //   : `${this.tipoSeleccionado}_${tipoAmortizacion}`;
  //   // // 3. Estructurar el objeto exactamente como lo espera tu tabla SQL
  //   // 1. Definir el prefijo basado en el botón presionado
  //   const prefijo = tipo === 'AYUDA' ? 'AYUDA - ' : `AMORTIZACION ${tipo} - `;

  //   // 2. Limpiar el comentario anterior si ya tenía un prefijo (para evitar duplicados al editar)
  //   let comentarioLimpio = formValues.comentario || '';
  //   if (comentarioLimpio.includes(' - ')) {
  //     comentarioLimpio = comentarioLimpio.split(' - ').slice(1).join(' - ');
  //   }
  //   const datosAEnviar = {
  //     idprestamos: formValues.idPrestamos,
  //     tipo: this.tipoSeleccionado || formValues.tipoPrestamo || 'PRESTAMO',
  //     idsocio: formValues.idSocio?.idsocio || formValues.idSocio,
  //     nombresocio: formValues.nombreSocio,
  //     fechaprestamo: formValues.fechaPrestamo,     // Mapeo a minúsculas
  //     montoprestado: Number(formValues.montoPrestado),
  //     plazoprestamo: Number(formValues.plazoPrestamo),
  //     interesprestamo: Number(formValues.interesPrestamo),
  //     interesmensual: Number(formValues.interesMensual),
  //     interestotal: Number(formValues.interesTotal),
  //     amortizacion: Number(formValues.amortizacion),
  //     cuota: Number(formValues.cuota),
  //     total: Number(formValues.total),
  //     // CONCATENACIÓN AQUÍ:
  //     comentario: prefijo + comentarioLimpio,
  //     tipoAmortizacion: tipo // Aquí guardas si es FAMILIAR, FRANCESA o ALEMANA

  //   };

  //   if (this.esEdicion) {
  //     // --- LÓGICA DE ACTUALIZACIÓN (PUT) ---
  //     const idParam = this.route.snapshot.paramMap.get('id');
  //     if (idParam) {
  //       this.sociosService.actualizarPrestamo(+idParam, datosAEnviar).subscribe({
  //         next: (res) => {
  //           alert('Prestamo actualizado correctamente');
  //           this.router.navigate(['/listaprestamos']);
  //         },
  //         error: (err) => {
  //           console.error('Error al actualizar:', err);
  //           alert('Error al actualizar el prestamo.');
  //         }
  //       });
  //     }
  //   } else {
  //     // --- LÓGICA DE CREACIÓN (POST) ---
  //     this.sociosService.registrarPrestamo(datosAEnviar).subscribe({
  //       next: (res) => {
  //         alert('Prestamo guardado correctamente');
  //         this.limpiarCampos();
  //       },
  //       error: (err) => {
  //         console.error('Error al guardar:', err);
  //         // Esto previene el error 500 al enviar datos limpios
  //         alert('No se pudo guardar el prestamo. Revise que el ID no esté duplicado.');
  //       }
  //     });
  //   }
  // }

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

  registrarPrestamo(tipo: string) {
    if (this.addPrestamoForm.invalid) return;

    const formValues = this.addPrestamoForm.getRawValue();

    // 1. Identificar el sufijo según el tipo de amortización
    // FAMILIAR (o AYUDA) usa las variables normales
    // FRANCESA usa las terminadas en '2'
    // ALEMANA usa las terminadas en '3'
    let sufijo = '';
    if (tipo === 'FRANCESA') sufijo = '2';
    if (tipo === 'ALEMANA') sufijo = '3';

    // 2. Definir el prefijo del comentario
    const prefijo = tipo === 'AYUDA' ? 'AYUDA - ' : `AMORTIZACION ${tipo} - `;
    let comentarioLimpio = formValues.comentario || '';
    if (comentarioLimpio.includes(' - ')) {
      comentarioLimpio = comentarioLimpio.split(' - ').slice(1).join(' - ');
    }

    // 1. Extraer la fecha del formulario
    const rawDate = this.addPrestamoForm.value.fechaPrestamo;

    // 2. Convertir a string "YYYY-MM-DD" ignorando horas
    const fechaLimpia = this.formatDate(rawDate);

    // 3. Mapear dinámicamente los valores según el sufijo
    const datosAEnviar = {
      idprestamos: formValues.idPrestamos,
      tipo: this.tipoSeleccionado || formValues.tipoPrestamo || 'PRESTAMO',
      idsocio: formValues.idSocio?.idsocio || formValues.idSocio,
      nombresocio: formValues.nombreSocio,
      fechaprestamo: fechaLimpia,
      montoprestado: Number(formValues.montoPrestado),
      plazoprestamo: Number(formValues.plazoPrestamo),
      interesprestamo: Number(formValues.interesPrestamo),

      // Aquí usamos la clave dinámica [nombre + sufijo]
      interesmensual: Number(formValues['interesMensual' + sufijo]),
      interestotal: Number(formValues['interesTotal' + sufijo]),
      amortizacion: Number(formValues['amortizacion' + sufijo]),
      cuota: Number(formValues['cuota' + sufijo]),
      total: Number(formValues['total' + sufijo]),

      comentario: prefijo + comentarioLimpio,
      tipoAmortizacion: tipo
    };

    // --- Lógica de guardado (Edición o Creación) ---
    if (this.esEdicion) {
      const idParam = this.route.snapshot.paramMap.get('id');
      if (idParam) {
        this.sociosService.actualizarPrestamo(+idParam, datosAEnviar).subscribe({
          next: (res) => {
            alert('Préstamo actualizado correctamente');
            this.router.navigate(['/litaprestamos']);
          },
          error: (err) => console.error('Error al actualizar:', err)
        });
      }
    } else {
      this.sociosService.registrarPrestamo(datosAEnviar as any).subscribe({
        next: (res) => {
          alert('Préstamo guardado correctamente');
          // DISPARA EL PDF AQUÍ (Antes de limpiar los campos)
          this.dispararPDFSegunTipo(tipo);
          this.limpiarCampos();
        },
        error: (err) => console.error('Error al guardar:', err)
      });
    }
  }

  // Crea esta función pequeña para organizar mejor el código:
  dispararPDFSegunTipo(tipo: string) {
    switch (tipo) {
      case 'FAMILIAR': this.exportarTablaPDF(1); break;
      case 'FRANCESA': this.exportarTablaPDF(2); break;
      case 'ALEMANA': this.exportarTablaPDF(3); break;
    }
  }

  lanzarAlerta() {
    this.snackBar.open('⚠️ Elige la forma de financiación (PRESTAMO o AYUDA) para habilitar el formulario', 'Cerrar', {
      duration: 4000,
      verticalPosition: 'top',
      panelClass: ['warning-snackbar']
    });
  }

  private _filter(value: string): Socio[] {
    const filterValue = value.toLowerCase();
    return this.sociosDisponibles.filter(socio =>
      socio.nombresocio?.toLowerCase().includes(filterValue) ||
      // Usamos .idsocio porque así está en Socio.ts
      socio.idsocio?.toString().includes(filterValue)
    );
  }

  displaySocioFn(socio: any): string {
    if (!socio) return '';
    // Si es el objeto Socio, retornamos su id
    return socio.idsocio ? socio.idsocio.toString() : socio.toString();
  }

  // Evento al seleccionar una opción del Autocomplete
  onSocioSelected(event: MatAutocompleteSelectedEvent): void {
    const socioSeleccionado: Socio = event.option.value;

    // Sincroniza el campo de nombre para que el usuario lo vea
    this.addPrestamoForm.get('nombreSocio')!.setValue(socioSeleccionado.nombresocio);
  }

  irASocio(): void {
    this.router.navigate(['/socios']);
  }

  irListaPrest(): void {
    this.router.navigate(['/litaprestamos']);
  }

  limpiarCampos(): void {
    // 1. Usa el método reset() en el FormGroup
    this.addPrestamoForm.reset();
    // this.tipoSeleccionado = null;
    this.tipoSeleccionado2 = null;
    this.obtenerNuevoId(); // Para obtener un nuevo ID después de limpiar
  }

  // Función auxiliar para no repetir código
  obtenerNuevoId() {
    // this.sociosService.getSiguienteIdPrestamo(this.tipoSeleccionado || 'PRESTAMO').subscribe(res => {
    //   this.addPrestamoForm.patchValue({
    //     idPrestamos: res.siguienteId
    //   });
    // });
    this.sociosService.getSiguienteIdPrestamo().subscribe(res => {
      this.addPrestamoForm.patchValue({
        idPrestamos: res.siguienteId
      });
    });
  }

  // cargarDatosPrestamo(id: string) {
  //   this.sociosService.getPrestamoById(id).subscribe({
  //     next: (res) => {
  //       if (res) {
  //         // CAMBIO CRÍTICO: Usar s.id para que coincida con tu modelo
  //         const socioCompleto = this.sociosDisponibles.find(s =>
  //           String(s.idsocio) === String(res.idsocio)
  //         );

  //         this.addPrestamoForm.patchValue({
  //           idPrestamos: res.idprestamos,
  //           tipoPrestamo: res.tipo,
  //           idSocio: socioCompleto ? socioCompleto : res.idsocio,
  //           nombreSocio: socioCompleto ? socioCompleto.nombresocio : '', // Campo solo para mostrar el nombre, no editable
  //           fechaPrestamo: res.fechaprestamo,
  //           montoPrestado: res.montoprestado,
  //           plazoPrestamo: res.plazoprestamo,
  //           interesPrestamo: res.interesprestamo,
  //           interesMensual: res.interesmensual,
  //           interesTotal: res.interestotal,
  //           amortizacion: res.amortizacion,
  //           cuota: res.cuota,
  //           total: res.total,
  //           comentario: res.comentario

  //         });
  //         // Forzar el cálculo de la tabla después de cargar los datos
  //         this.calculateLoanDetails(this.addPrestamoForm.getRawValue());
  //       }
  //     },
  //     error: (err) => console.error('Error al cargar datos:', err)
  //   });
  // }

  cargarDatosPrestamo(id: string) {
    this.sociosService.getPrestamoById(id).subscribe({
      next: (res) => {
        if (res) {
          // const tipoOriginal = res.tipo || ''; // Viene como "PRESTAMO_FAMILIAR" o "AYUDA"

          // if (tipoOriginal.includes('_')) {
          //   const partes = tipoOriginal.split('_');
          //   this.tipoSeleccionado = partes[0]; // "PRESTAMO"
          //   this.tipoSeleccionado2 = partes[1] as any; // Usamos 'as any' o el tipado correcto
          // } else {
          //   this.tipoSeleccionado = tipoOriginal;
          //   // Si es AYUDA, lo mejor es dejar el segundo selector en null 
          //   // para que no interfiera con los botones de amortización
          //   this.tipoSeleccionado2 = (tipoOriginal === 'AYUDA') ? null : 'FAMILIAR';
          // }
          this.esEdicion = true;
          this.tipoSeleccionado = res.tipo;

          const comentarioSuperior = res.comentario ? res.comentario.toUpperCase() : '';

          // Detectar el botón basándose en el prefijo del comentario
          if (comentarioSuperior.includes('FAMILIAR')) {
            this.tipoSeleccionado2 = 'FAMILIAR';
          } else if (comentarioSuperior.includes('FRANCESA')) {
            this.tipoSeleccionado2 = 'FRANCESA';
          } else if (comentarioSuperior.includes('ALEMANA')) {
            this.tipoSeleccionado2 = 'ALEMANA';
          } else if (res.tipo === 'AYUDA') {
            this.tipoSeleccionado2 = 'AYUDA';
          }

          const socioCompleto = this.sociosDisponibles.find(s =>
            String(s.idsocio) === String(res.idsocio)
          );

          // Cuando recibes los datos del servidor para llenar el formulario:
          const fechaDeBaseDatos = res.fechaprestamo; // Viene como "2026-04-20" o "2026-04-20T00:00:00.000Z"

          // Forzamos a que cree la fecha sin desfase
          const dateParts = fechaDeBaseDatos.split('T')[0].split('-');
          const fechaParaElForm = new Date(
            Number(dateParts[0]),
            Number(dateParts[1]) - 1,
            Number(dateParts[2])
          );

          this.addPrestamoForm.patchValue({
            idPrestamos: res.idprestamos,
            tipoPrestamo: res.tipo,
            idSocio: socioCompleto ? socioCompleto : res.idsocio,
            nombreSocio: socioCompleto ? socioCompleto.nombresocio : '',
            // fechaPrestamo: res.fechaprestamo,
            fechaPrestamo: fechaParaElForm,
            montoPrestado: res.montoprestado,
            plazoPrestamo: res.plazoprestamo,
            interesPrestamo: res.interesprestamo,
            interesMensual: res.interesmensual,
            interesTotal: res.interestotal,
            amortizacion: res.amortizacion,
            cuota: res.cuota,
            total: res.total,
            comentario: res.comentario
          });

          // 2. Si es AYUDA, ajustamos validadores para que el formulario sea válido
          if (this.tipoSeleccionado === 'PRESTAMO') {
            this.addPrestamoForm.get('plazoPrestamo')?.setValidators([Validators.required, Validators.min(1)]);
            this.addPrestamoForm.get('interesPrestamo')?.setValidators([Validators.required]);
            this.addPrestamoForm.get('plazoPrestamo')?.enable();
            this.addPrestamoForm.get('interesPrestamo')?.enable();
          } else if (this.tipoSeleccionado === 'AYUDA') {
            // Tu lógica actual de clearValidators para AYUDA
            this.addPrestamoForm.get('plazoPrestamo')?.clearValidators();
            this.addPrestamoForm.get('interesPrestamo')?.clearValidators();
            this.addPrestamoForm.get('plazoPrestamo')?.disable();
            this.addPrestamoForm.get('interesPrestamo')?.disable();
          }

          // this.addPrestamoForm.updateValueAndValidity();

          this.calculateLoanDetails(this.addPrestamoForm.getRawValue());
        }
      },
      error: (err) => console.error('Error al cargar datos:', err)
    });
  }

  seleccionarTipo(tipo: 'PRESTAMO' | 'AYUDA') {
    this.tipoSeleccionado = tipo;
    this.addPrestamoForm.get('tipoPrestamo')?.setValue(tipo); // <--- ESTO ES CLAVE

    // 1. Obtener el siguiente ID (Tu función actual)
    this.sociosService.getSiguienteIdPrestamo().subscribe(res => {
      this.addPrestamoForm.patchValue({
        idPrestamos: res.siguienteId
      });
    });

    // 2. Lógica específica para AYUDA
    if (tipo === 'AYUDA') {
      this.tipoSeleccionado2 = null; // Limpiar selección de tipo de amortización
      this.addPrestamoForm.patchValue({
        // tipoPrestamo: 'AYUDA',
        plazoPrestamo: 0, // Forzamos plazo a 1 mes para AYUDA
        interesPrestamo: 0, // Forzamos interés a 0 en el formulario
        interesMensual: 0,
        interesTotal: 0,
        amortizacion: 0,
        cuota: 0,
        // total: this.addPrestamoForm.get('montoPrestado')?.value || 0 // El total a pagar es igual al monto prestado

      }, { emitEvent: true }); // emitEvent: true dispara el cálculo automático

      // Opcional: Bloquear el campo para que no lo editen
      this.addPrestamoForm.get('interesPrestamo')?.disable();
      this.addPrestamoForm.get('plazoPrestamo')?.disable();
    } else {
      this.tipoSeleccionado2 = null; // Limpiar selección de tipo de amortización
      // this.addPrestamoForm.get('tipoPrestamo')?.setValue('PRESTAMO');
      // Si vuelve a PRÉSTAMO, habilitamos y limpiamos
      this.addPrestamoForm.get('interesPrestamo')?.enable();
      this.addPrestamoForm.get('plazoPrestamo')?.enable();
      this.addPrestamoForm.patchValue({ interesPrestamo: '' });
    }
  }

  seleccionarTipo2(tipo: 'FAMILIAR' | 'FRANCESA' | 'ALEMANA') {
    this.tipoSeleccionado2 = tipo;
  }


  ngOnDestroy(): void {
    // Limpia la suscripción cuando el componente se destruye
    this.destroy$.next();
    this.destroy$.complete();
  }

  setupCalculationListener(): void {
    // Combina todos los controles de entrada clave en un solo observable
    this.addPrestamoForm.valueChanges
      .pipe(
        // Opcional: si el formulario es muy grande, puedes usar debounceTime para evitar cálculos excesivos
        // debounceTime(300), 
        takeUntil(this.destroy$)
      )
      .subscribe(values => {
        // Llama a la función de cálculo cada vez que cambie un valor
        this.calculateLoanDetails(values);
      });
  }


  generateAmortizationTableFamiliar(monto: number, plazoMeses: number, interesMensualTasa: number, cuota: number): AmortizacionRow[] {
    const tabla: AmortizacionRow[] = [];
    let saldoActual = monto;
    const fechaOriginal = new Date(this.addPrestamoForm.value.fechaPrestamo);

    // Recorrer todos los períodos del préstamo
    for (let i = 1; i <= plazoMeses; i++) {
      // 2. CREAMOS una copia de la fecha y sumamos meses ADENTRO del bucle
      const fechaPago = new Date(fechaOriginal);
      fechaPago.setMonth(fechaPago.getMonth() + i);
      const interesMes = interesMensualTasa * 100;
      const amortizacionMes = cuota + interesMes;
      const saldoFinal = saldoActual - cuota;

      tabla.push({
        periodo: i,
        fechaPago: fechaPago.toLocaleDateString('es-EC'), // Formato Ecuador: DD/MM/AAAA
        saldoInicial: saldoActual,
        cuota: cuota,
        interes: interesMes,
        amortizacion: amortizacionMes,
        saldoFinal: (saldoFinal < 0.01 && saldoFinal > -0.01) ? 0 : saldoFinal // Corrige pequeños errores de punto flotante
      });

      saldoActual = saldoFinal;
    }
    // Ejemplo para el Sistema Familiar (haz lo mismo para los otros)
    this.totalesTabla1 = {
      interesTotal: tabla.reduce((sum, item) => sum + item.interes, 0),
      amortizacionTotal: tabla.reduce((sum, item) => sum + item.amortizacion, 0),
      cuotaTotal: tabla.reduce((sum, item) => sum + item.cuota, 0)
    };
    return tabla;
  }

  generateAmortizationTableFrancesa(monto: number, plazoMeses: number, tasaMensualDecimal: number, cuota: number): AmortizacionRow[] {
    const tabla: AmortizacionRow[] = [];
    let saldoActual = monto;
    const fechaOriginal = new Date(this.addPrestamoForm.value.fechaPrestamo);

    for (let i = 1; i <= plazoMeses; i++) {
      const fechaPago = new Date(fechaOriginal);
      fechaPago.setMonth(fechaPago.getMonth() + i);
      const interesMes = saldoActual * tasaMensualDecimal; // Interés sobre saldo
      const amortizacionMes = cuota - interesMes; // Lo que sobra de la cuota va al capital
      const saldoFinal = saldoActual - amortizacionMes; // Solo la amortización resta al saldo

      tabla.push({
        periodo: i,
        fechaPago: fechaPago.toLocaleDateString('es-EC'), // Formato Ecuador: DD/MM/AAAA
        saldoInicial: saldoActual,
        cuota: cuota,
        interes: interesMes,
        amortizacion: amortizacionMes,
        saldoFinal: Math.max(0, saldoFinal)
      });

      saldoActual = saldoFinal;
    }
    // Ejemplo para el Sistema Familiar (haz lo mismo para los otros)
    this.totalesTabla2 = {
      interesTotal: tabla.reduce((sum, item) => sum + item.interes, 0),
      amortizacionTotal: tabla.reduce((sum, item) => sum + item.amortizacion, 0),
      cuotaTotal: tabla.reduce((sum, item) => sum + item.cuota, 0)
    };
    return tabla;
  }

  generateAmortizationTableAlemana(monto: number, plazoMeses: number, tasaMensualDecimal: number, amortizacionFija: number): AmortizacionRow[] {
    const tabla: AmortizacionRow[] = [];
    let saldoActual = monto;
    const fechaOriginal = new Date(this.addPrestamoForm.value.fechaPrestamo);

    for (let i = 1; i <= plazoMeses; i++) {
      const fechaPago = new Date(fechaOriginal);
      fechaPago.setMonth(fechaPago.getMonth() + i);
      const interesMes = saldoActual * tasaMensualDecimal; // Interés sobre saldo
      const cuotaMes = amortizacionFija + interesMes; // Cuota decreciente
      const saldoFinal = saldoActual - amortizacionFija;

      tabla.push({
        periodo: i,
        fechaPago: fechaPago.toLocaleDateString('es-EC'), // Formato Ecuador: DD/MM/AAAA
        saldoInicial: saldoActual,
        cuota: cuotaMes,
        interes: interesMes,
        amortizacion: amortizacionFija,
        saldoFinal: Math.max(0, saldoFinal)
      });

      saldoActual = saldoFinal;
    }
    // Ejemplo para el Sistema Familiar (haz lo mismo para los otros)
    this.totalesTabla3 = {
      interesTotal: tabla.reduce((sum, item) => sum + item.interes, 0),
      amortizacionTotal: tabla.reduce((sum, item) => sum + item.amortizacion, 0),
      cuotaTotal: tabla.reduce((sum, item) => sum + item.cuota, 0)
    };
    return tabla;
  }

  // calcularTotales(tabla: AmortizacionRow[]) {
  //   return tabla.reduce((acc, row) => {
  //     acc.cuota += row.cuota;
  //     acc.interes += row.interes;
  //     acc.amortizacion += row.amortizacion;
  //     return acc;
  //   }, { cuota: 0, interes: 0, amortizacion: 0 });
  // }

  calcularTotales(tabla: AmortizacionRow[]): TotalesAmortizacion {
    return {
      interesTotal: tabla.reduce((sum, item) => sum + (item.interes || 0), 0),
      amortizacionTotal: tabla.reduce((sum, item) => sum + (item.amortizacion || 0), 0),
      cuotaTotal: tabla.reduce((sum, item) => sum + (item.cuota || 0), 0)
    };
  }

  // // Variables para almacenar los totales
  // totalesTabla1 = { cuota: 0, interes: 0, amortizacion: 0 };
  // totalesTabla2 = { cuota: 0, interes: 0, amortizacion: 0 };
  // totalesTabla3 = { cuota: 0, interes: 0, amortizacion: 0 };

  /**
   * Función que realiza los cálculos principales del préstamo.
   * @param formValues Los valores actuales del FormGroup.
   */
  calculateLoanDetails(formValues: any): void {
    const monto = parseFloat(formValues.montoPrestado);
    const esAyuda = this.tipoSeleccionado === 'AYUDA';

    if (esAyuda) {
      // Si es ayuda, forzamos que el total sea igual al monto y lo demás 0
      this.addPrestamoForm.patchValue({
        interesMensual: '0.00',
        interesTotal: '0.00',
        amortizacion: '0.00',
        cuota: '0.00',
        total: monto.toFixed(2) // AQUÍ es donde se "trae" el monto dinámicamente
      }, { emitEvent: false });

      this.tablaAmortizacion1 = []; // No suele haber tabla para ayudas de un solo pago
      return; // Salimos de la función para no ejecutar la lógica de préstamos
    }
    const plazoMeses = parseInt(formValues.plazoPrestamo, 10);
    // Si es AYUDA, forzamos interés 0; si no, usamos el del formulario
    const interesAnualPerc = this.tipoSeleccionado === 'AYUDA' ? 0 : (parseFloat(formValues.interesPrestamo) || 0);
    const interesAnualDecimal = interesAnualPerc / 100;

    // Se asegura de que los campos clave sean válidos y positivos antes de calcular
    if (monto > 0 && plazoMeses > 0) {
      // 1. CALCULAR VOLORES DE AMORTIZACION FAMILIAR
      const interesMensualTasa = interesAnualDecimal * monto / 100; // Tasa efectiva mensual
      // 2. CALCULAR CUOTA FIJA
      const cuota = monto / plazoMeses + (monto * interesAnualDecimal);
      const amort = monto / plazoMeses;
      // 3. CALCULAR INTERÉS TOTAL (simplificado)
      const totalPagar = cuota * plazoMeses;
      const interesTotalCalculado = interesMensualTasa * 100 * plazoMeses;

      // 1. CALCULAR VOLORES DE AMORTIZACION FRACESA
      const interesPrimerMes2 = monto * (interesAnualPerc / 100);
      // 2. CALCULAR CUOTA FIJA (Fórmula de Amortización Francesa)
      // Cuota = P * [ i * (1 + i)^n ] / [ (1 + i)^n - 1 ]
      const cuota2 = monto * (interesAnualDecimal * Math.pow(1 + interesAnualDecimal, plazoMeses)) /
        (Math.pow(1 + interesAnualDecimal, plazoMeses) - 1);
      const amortPrimerMes2 = cuota2 - interesPrimerMes2;
      // 3. CALCULAR INTERÉS TOTAL (simplificado)
      const totalPagar2 = cuota2 * plazoMeses;
      const interesTotalCalculado2 = totalPagar2 - monto;

      // 1. CALCULAR VOLORES DE AMORTIZACION ALEMANA
      const interesMensualTasa3 = interesAnualDecimal * monto; // Tasa efectiva mensual
      // 2. CALCULAR CUOTA FIJA (Fórmula de Amortización Francesa)
      const amort3 = monto / plazoMeses;
      const interesPrimerMes = monto * interesAnualDecimal;
      const interesUltimoMes = amort3 * interesAnualDecimal;
      const cuota3 = amort3 + interesMensualTasa3;
      const interesTotalCalculado3 = ((interesPrimerMes + interesUltimoMes) * plazoMeses) / 2;
      // 3. CALCULAR INTERÉS TOTAL (simplificado)
      const totalPagar3 = monto + interesTotalCalculado3;

      // 4. ACTUALIZAR LOS CAMPOS DE RESULTADO
      this.addPrestamoForm.patchValue({
        interesMensual: (interesMensualTasa * 100).toFixed(2), // Mostramos la tasa mensual en %
        cuota: cuota.toFixed(2), // Redondeamos a 2 decimales
        interesTotal: interesTotalCalculado.toFixed(2),
        amortizacion: amort.toFixed(2), // Redondeamos a 2 decimales
        total: totalPagar.toFixed(2),
        // Nota: La amortización se calcula dentro de una tabla, no como un campo simple

        interesMensual2: interesPrimerMes2.toFixed(2), // Mostramos el interés del mes 1
        cuota2: cuota2.toFixed(2),
        interesTotal2: interesTotalCalculado2.toFixed(2),
        amortizacion2: amortPrimerMes2.toFixed(2),   // Mostramos la amortización del mes 1
        total2: totalPagar2.toFixed(2),

        interesMensual3: interesPrimerMes2.toFixed(2), // Mostramos el interés del mes 1
        cuota3: cuota3.toFixed(2),
        interesTotal3: interesTotalCalculado3.toFixed(2),
        amortizacion3: amort3.toFixed(2),   // Mostramos la amortización del mes 1
        total3: totalPagar3.toFixed(2),
      }, { emitEvent: false }); // Usamos { emitEvent: false } para evitar un loop infinito
      // 5. ¡NUEVO! GENERAR LA TABLA DE AMORTIZACIÓN
      this.tablaAmortizacion1 = this.generateAmortizationTableFamiliar(
        monto,
        plazoMeses,
        interesMensualTasa,
        amort
      );
      this.tablaAmortizacion2 = this.generateAmortizationTableFrancesa(
        monto,
        plazoMeses,
        interesAnualDecimal,
        cuota2
      );
      this.tablaAmortizacion3 = this.generateAmortizationTableAlemana(
        monto,
        plazoMeses,
        interesAnualDecimal,
        amort3
      );

      // Dentro de calculateLoanDetails, después de generar las tablas:
      // this.totalesTabla1 = this.calcularTotales(this.tablaAmortizacion1);
      // this.totalesTabla2 = this.calcularTotales(this.tablaAmortizacion2);
      // this.totalesTabla3 = this.calcularTotales(this.tablaAmortizacion3);
      // ... después de generar las tablas en calculateLoanDetails ...

      if (this.tablaAmortizacion1.length > 0) {
        this.totalesTabla1 = this.calcularTotales(this.tablaAmortizacion1);
      }
      if (this.tablaAmortizacion2.length > 0) {
        this.totalesTabla2 = this.calcularTotales(this.tablaAmortizacion2);
      }
      if (this.tablaAmortizacion3.length > 0) {
        this.totalesTabla3 = this.calcularTotales(this.tablaAmortizacion3);
      }

    } else {
      // Limpiar los campos de resultado si la entrada no es válida
      this.addPrestamoForm.patchValue({
        interesMensual: '',
        cuota: '',
        interesTotal: '',
        total: ''
      }, { emitEvent: false });
      this.tablaAmortizacion1 = [];
      this.tablaAmortizacion2 = [];
      this.tablaAmortizacion3 = [];
      // Limpiar la tabla si la entrada no es válida
    }
  }

  exportarTablaPDF(tipo: number): void {
    const doc = new jsPDF();
    let tablaSeleccionada: any[] = [];
    let nombreMetodo = '';
    let totales: any = null;
    const form = this.addPrestamoForm.value;

    // 1. Selección de datos (Asegúrate de que los totales se calculen antes de llamar esta función)
    switch (tipo) {
      case 1:
        tablaSeleccionada = this.tablaAmortizacion1;
        nombreMetodo = 'SISTEMA DE AMORTIZACIÓN FAMILIAR';
        totales = this.totalesTabla1;
        break;
      case 2:
        tablaSeleccionada = this.tablaAmortizacion2;
        nombreMetodo = 'SISTEMA DE AMORTIZACIÓN FRANCÉS';
        totales = this.totalesTabla2;
        break;
      case 3:
        tablaSeleccionada = this.tablaAmortizacion3;
        nombreMetodo = 'SISTEMA DE AMORTIZACIÓN ALEMÁN';
        totales = this.totalesTabla3;
        break;
    }

    // 2. Encabezado del Documento
    doc.setFontSize(16);
    doc.setTextColor(8, 145, 178);
    doc.text('CAJA DE AHORRO FAMILIA UNIDA', 105, 15, { align: 'center' });

    doc.setFontSize(10);
    doc.setTextColor(100);
    doc.text(nombreMetodo, 105, 22, { align: 'center' });

    // 3. Información del Préstamo
    doc.setFontSize(9);
    doc.setTextColor(0);

    // Fila 1: Socio y Fecha de Emisión
    doc.text(`Socio: ${form.nombreSocio || 'N/A'}`, 14, 32);
    doc.text(`Fecha Emisión: ${new Date().toLocaleDateString()}`, 150, 32);

    // Fila 2: Número de Préstamo (Bajamos a 38 para que no choque con Socio)
    doc.text(`Número de Préstamo: ${form.idPrestamos || 'N/A'}`, 14, 38);

    // Fila 3: Monto
    doc.text(`Monto: $${parseFloat(form.montoPrestado).toFixed(2)}`, 14, 44);

    // Fila 4: Interés y Plazo
    doc.text(`Interés: ${form.interesPrestamo}% Mensual`, 14, 50);
    doc.text(`Plazo: ${form.plazoPrestamo} meses`, 70, 50);

    // Fila 5: Observaciones
    doc.text(`Observaciones: ${form.comentario || 'N/A'}`, 14, 56);

    // 4. Generación de la Tabla con autoTable
    // Definimos la fila inicial (Periodo 0)
    const filaInicial = [
      '0',                                     // # Periodo
      '---',             // Fecha inicial
      '',                                      // Interés (vacío)
      '',                                      // Cuota Total (vacío)
      '',                                      // Capital (vacío)
      `$${Number(form.montoPrestado).toFixed(2)}` // Saldo Final = Monto Total
    ];
    // Y CÁMBIALA POR ESTA:
    autoTable(doc, {
      startY: 62,
      // Columnas: 0:Cuota, 1:Fecha, 2:Interés, 3:Cuota Total, 4:Capital, 5:Saldo
      head: [['#', 'Fecha', 'Interés', 'Capital', 'Cuota Total', 'Saldo']],
      body: [
        filaInicial,
        ...tablaSeleccionada.map(fila => [
          fila.periodo,
          fila.fechaPago || 'Pendiente',
          `$${Number(fila.interes).toFixed(2)}`,
          `$${Number(fila.cuota).toFixed(2)}`,
          `$${Number(fila.amortizacion).toFixed(2)}`,
          `$${Number(fila.saldoFinal).toFixed(2)}`
        ])
      ],
      foot: [[
        '', // 0: Cuota (Vacio)
        'TOTALES', // 1: Fecha (Etiqueta)
        `$${Number(totales?.interesTotal || 0).toFixed(2)}`,    // 2: Bajo Interés
        `$${Number(totales?.cuotaTotal || 0).toFixed(2)}`,      // 3: Bajo Cuota Total
        `$${Number(totales?.amortizacionTotal || 0).toFixed(2)}`, // 4: Bajo Capital
        ''  // 5: Bajo Saldo (Vacio)
      ]],
      theme: 'striped',
      headStyles: { fillColor: [8, 145, 178], halign: 'center' },
      columnStyles: {
        0: { halign: 'center' },
        1: { halign: 'center' },
        2: { halign: 'right' }, // Interés alineado a la derecha
        3: { halign: 'right' }, // Cuota Total alineada a la derecha
        4: { halign: 'right' }, // Capital alineado a la derecha
        5: { halign: 'right' }  // Saldo alineado a la derecha
      },
      // Corregido textColor: [0, 0, 0] para evitar error ts(2322)
      footStyles: {
        fillColor: [241, 245, 249],
        textColor: [0, 0, 0],
        fontStyle: 'bold',
        halign: 'right' // Fuerza a que todo el contenido del pie se alinee a la derecha
      }
    });

    // 5. Sección de Firmas (posicionada después de la tabla)
    const finalY = (doc as any).lastAutoTable.finalY + 30;
    doc.line(30, finalY, 80, finalY);
    doc.text('Firma Tesorero', 55, finalY + 5, { align: 'center' });

    doc.line(130, finalY, 180, finalY);
    doc.text('Firma Socio', 155, finalY + 5, { align: 'center' });

    // 6. Guardar el archivo
    doc.save(`Tabla_${form.nombreSocio}.pdf`);
  }

}

