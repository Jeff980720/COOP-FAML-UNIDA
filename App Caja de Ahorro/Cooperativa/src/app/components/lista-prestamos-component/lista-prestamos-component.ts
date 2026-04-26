import { Component } from '@angular/core';
import { Prestamos } from '../../models/Prestamos';
import { Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import jsPDF from 'jspdf';
import autoTable from 'jspdf-autotable';
import { EstadoResultado } from '../estado-resultado/estado-resultado';
import { FormBuilder, FormGroup, ReactiveFormsModule } from '@angular/forms';
import { map, Observable, startWith } from 'rxjs';
import { Socio } from '../../models/Socio';
import { MatAutocompleteModule, MatAutocompleteSelectedEvent } from '@angular/material/autocomplete';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatTableModule } from '@angular/material/table';
import { MatButtonModule } from '@angular/material/button';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { BaseChartDirective } from 'ng2-charts';
import { MatInputModule } from '@angular/material/input';
import { SociosService } from '../../services/socios';

@Component({
  selector: 'app-lista-prestamos-component',
  // standalone: true,
  imports: [CommonModule,
    MatIconModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatAutocompleteModule,
    MatButtonModule,
    MatDatepickerModule,
    MatNativeDateModule,
    MatIconModule, // <--- 2. AGRÉGALO AQUÍ
    MatTableModule, // <--- AGREGAR AQUÍ
  ],
  templateUrl: './lista-prestamos-component.html',
  styleUrl: './lista-prestamos-component.css',
  providers: [EstadoResultado] // Lo agregas como proveedor aquí
})
export class ListaPrestamosComponent {

  prest: Prestamos[] = [];
  // DEBES DECLARAR LA VARIABLE AQUÍ
  public listaPrestamos: any[] = [];

  dashboardForm!: FormGroup; // Usar el nombre correcto consistentemente
  filteredSocios!: Observable<Socio[]>;
  listaSocios: any[] = [];
  sociosDisponibles: Socio[] = [];

  fechaInicioPeriodo: string = "";
  // fechaInicioOperacion: string = "";
  fechaFinPeriodo: string = "";

  constructor(private service: SociosService, private router: Router, private estadoResultados: EstadoResultado, private fb: FormBuilder) { }

  ngOnInit(): void {
    // 1. Cargar préstamos iniciales
    // this.service.getPrestamos().subscribe({
    //   next: (data) => {
    //     this.prest = data;
    //     // IMPORTANTE: Si ya tenemos fechas en el servicio al entrar, filtramos de una vez
    //     if (this.service.fechaInicioOperacion || this.service.fechaFinPeriodo) {
    //       this.filtrarPrestamos();
    //     }
    //   },
    //   error: (err) => console.error('Error:', err)
    // });

    this.service.getPrestamos().subscribe(
      (data: any) => {
        // OPCIÓN A: Si quieres mantener la estructura de 'Coperativa'
        this.prest = data; // Asignamos directamente el arreglo de aportes
        console.log('Datos mapeados:', this.prest);
      },
      (error) => {
        console.error('Error:', error);
      }
    );

    this.cargarDatos();

    // 2. Inicializar formulario con valores del servicio
    this.dashboardForm = this.fb.group({
      idSocio: [null],
      nombreSocio: [''],
      fechaInicio: [this.service.fechaInicioPeriodo2],
      fechaFin: [this.service.fechaFinPeriodo2]
    });

    // 3. UNA SOLA suscripción para persistir y filtrar
    this.dashboardForm.valueChanges.subscribe(valores => {
      // Persistimos en el servicio
      this.service.fechaInicioPeriodo2 = valores.fechaInicio;
      this.service.fechaFinPeriodo2 = valores.fechaFin;

      // Lógica de limpieza: Si los campos están vacíos, recargamos datos globales
      if (!valores.idSocio && !valores.fechaInicio && !valores.fechaFin) {
        this.cargarDatos();
      } else {
        // Si hay datos (incluyendo las fechas que acabas de recuperar), filtramos
        this.filtrarPrestamos();
      }
    });

    // 4. EJECUCIÓN INICIAL: Esto evita que se pierda el filtro al cambiar de ruta
    // Si al entrar ya existen fechas, forzamos el primer filtrado
    if (this.service.fechaInicioPeriodo2 || this.service.fechaFinPeriodo2) {
      // Usamos un pequeño timeout para asegurar que los datos de 'prest' ya existan
      setTimeout(() => this.filtrarPrestamos(), 100);
    }
  }

  // En lista-aporte-component.ts

  // 1. ELIMINAR
  // eliminarPrestamo(idPrestamo: any): void {
  //   const password = prompt('Ingrese la contraseña de administrador para eliminar el préstamo:');

  //   if (password === '1998') {
  //     const confirmar = confirm(`¿Está seguro de eliminar el préstamo #${idPrestamo}?`);

  //     if (confirmar) {
  //       // IMPORTANTE: El objeto dentro de subscribe debe tener 'next' y 'error'
  //       this.service.eliminarPrestamo(idPrestamo).subscribe({
  //         next: (response) => {
  //           // ESTO SOLO SI EL SERVIDOR DICE 200 OK
  //           this.prest = this.prest.filter(a => a.idprestamos !== idPrestamo);
  //           alert('Registro eliminado correctamente.');
  //         },
  //         error: (err) => {
  //           // ESTO ES LO QUE DEBE SALIR CON EL ERROR 400
  //           console.error('Error detectado:', err);
  //           const msg = err.error?.message || 'No se puede eliminar porque tiene pagos.';
  //           alert('ERROR: ' + msg);
  //         }
  //       });
  //     }
  //   } else if (password !== null) {
  //     alert('Contraseña incorrecta.');
  //   }
  // }

  eliminarPrestamo(idPrestamo: any): void {
    Swal.fire({
      title: 'Seguridad de Administrador',
      input: 'password',
      inputLabel: 'Ingrese la contraseña para eliminar:',
      showCancelButton: true,
      confirmButtonText: 'Validar',
      cancelButtonText: 'Cancelar'
    }).then((result) => {
      if (result.value === '1998') {
        // Si la clave es correcta, procedemos
        this.ejecutarEliminacion(idPrestamo);
      } else if (result.isConfirmed) {
        Swal.fire('Error', 'Contraseña incorrecta', 'error');
      }
    });
  }

  // Mueve la lógica del subscribe a una función aparte
  private ejecutarEliminacion(idPrestamo: any) {
    this.service.eliminarPrestamo(idPrestamo).subscribe({
      next: () => {
        this.prest = this.prest.filter(a => a.idprestamos !== idPrestamo);
        Swal.fire('Eliminado', 'Registro borrado con éxito', 'success');
      },
      error: (err) => {
        const msg = err.error?.message || 'No se puede eliminar porque tiene pagos.';
        Swal.fire('Error', msg, 'error');
      }
    });
  }

  // 2. ACTUALIZAR
  // editarPrestamo(prestamo: any) {
  //   console.log('Intentando editar el préstamo con ID:', prestamo.idprestamos);

  //   // VALIDACIÓN: Bloquear si es PAGADO, pero SOLO si NO es una AYUDA
  //   // La lógica es: (Está pagado) Y (NO empieza con AYUDA)
  //   const esAyuda = prestamo.tipo && prestamo.tipo.startsWith('AYUDA');

  //   if (prestamo.estatus_dinamico === 'PAGADO' && !esAyuda) {
  //     console.warn('Acceso denegado: El préstamo ya está pagado.');
  //     alert('Este préstamo ya se encuentra PAGADO y no puede ser editado.');
  //     return;
  //   }

  //   // Si es AYUDA o no está PAGADO, pide la contraseña
  //   const password = prompt('Ingrese la contraseña de administrador para editar:');

  //   if (password === '1998') {
  //     if (prestamo && prestamo.idprestamos) {
  //       this.router.navigate(['/editar-prestamos', prestamo.idprestamos])
  //         .then(nav => console.log('¿Navegación exitosa?:', nav))
  //         .catch(err => console.error('Error en navegación:', err));
  //     } else {
  //       console.error('ID no válido', prestamo);
  //     }
  //   } else if (password !== null) {
  //     alert('Contraseña incorrecta.');
  //   }
  // }

  editarPrestamo(prestamo: any) {
    console.log('Intentando editar el préstamo con ID:', prestamo.idprestamos);

    // 1. Lógica de validación (Se mantiene igual)
    const esAyuda = prestamo.tipo && prestamo.tipo.startsWith('AYUDA');

    if (prestamo.estatus_dinamico === 'PAGADO' && !esAyuda) {
      console.warn('Acceso denegado: El préstamo ya está pagado.');
      Swal.fire({
        icon: 'warning',
        title: 'Acceso Denegado',
        text: 'Este préstamo ya se encuentra PAGADO y no puede ser editado.',
        confirmButtonColor: '#3085d6'
      });
      return;
    }

    // 2. Sustitución de prompt por SweetAlert2 (Compatible con escritorio)
    Swal.fire({
      title: 'Validación de Seguridad',
      text: 'Ingrese la contraseña de administrador para editar:',
      input: 'password',
      inputAttributes: {
        autocapitalize: 'off'
      },
      showCancelButton: true,
      confirmButtonText: 'Confirmar',
      cancelButtonText: 'Cancelar',
      confirmButtonColor: '#28a745'
    }).then((result) => {
      if (result.isConfirmed) {
        if (result.value === '1998') {
          // 3. Navegación si la clave es correcta
          if (prestamo && prestamo.idprestamos) {
            this.router.navigate(['/editar-prestamos', prestamo.idprestamos])
              .then(nav => console.log('¿Navegación exitosa?:', nav))
              .catch(err => console.error('Error en navegación:', err));
          } else {
            console.error('ID no válido', prestamo);
          }
        } else {
          Swal.fire('Error', 'Contraseña incorrecta.', 'error');
        }
      }
    });
  }

  irASocio(): void {
    this.router.navigate(['/socios']);
  }

  irFormPrest(): void {
    this.router.navigate(['/prestamos']);
  }

  cargarDatos() {
    // Asegúrate de llenar la variable con los datos de tu servicio
    this.service.getPrestamos().subscribe(res => {
      this.listaPrestamos = res;
    });

    this.service.getSocios().subscribe({
      next: (data: any) => {
        const socios = Array.isArray(data) ? data : (data.items || []);
        this.listaSocios = socios;
        this.sociosDisponibles = socios;
        this.configurarFiltroSocios();
      },
      error: (err) => console.error('Error cargando socios:', err)
    });
  }

  configurarFiltroSocios() {
    this.filteredSocios = this.dashboardForm.get('idSocio')!.valueChanges.pipe(
      startWith(''),
      map(value => {
        // 1. Extraer el texto a filtrar
        // Si value es un objeto (seleccionado), usamos su nombresocio
        // Si es un string (editando), lo usamos directamente
        const filterText = typeof value === 'string' ? value : value?.nombresocio || '';

        // 2. Retornar la lista filtrada
        return filterText ? this._filter(filterText) : this.listaSocios.slice();
      })
    );
  }

  private _filter(value: string): Socio[] {
    const filterValue = value.toLowerCase();
    // Filtramos por nombre o por ID
    return this.listaSocios.filter(socio =>
      socio.nombresocio.toLowerCase().includes(filterValue) ||
      socio.idsocio.toString().includes(filterValue)
    );
  }

  onSocioSelected(event: MatAutocompleteSelectedEvent): void {
    const socioSeleccionado: Socio = event.option.value;
    const idSocio = socioSeleccionado.idsocio;

    this.dashboardForm.patchValue({
      idSocio: socioSeleccionado,
      nombreSocio: socioSeleccionado.nombresocio
    });

    this.filtrarPrestamos();
  }

  displaySocioFn(socio: any): string {
    // Cuando seleccionas, 'socio' es el objeto completo. 
    // Esta función extrae el string que se verá en el input.
    return socio && socio.nombresocio ? socio.nombresocio : '';
  }

  filtrarPrestamos(): void {
    // const idSocio = this.dashboardForm.get('idSocio')?.value?.idsocio;
    // const fechaInicio = this.dashboardForm.get('fechaInicio')?.value;
    // const fechaFin = this.dashboardForm.get('fechaFin')?.value;

    const { idSocio, fechaInicio, fechaFin } = this.dashboardForm.value;

    const inicioStr = fechaInicio ? new Date(fechaInicio).toISOString().split('T')[0] : undefined;
    const finStr = fechaFin ? new Date(fechaFin).toISOString().split('T')[0] : undefined;

    this.service.getPrestamosFiltrados(idSocio?.idsocio, inicioStr, finStr).subscribe({
      next: (data: any[]) => {
        this.listaPrestamos = data;
        console.log('Prestamos filtrados:', data);
      },
      error: (err) => console.error('Error filtrando préstamos:', err)
    });

  }

  botonDescargarPDF() {
    // Ahora 'listaPrestamos' ya existe y la función acepta 1 argumento
    this.estadoResultados.exportarListaPrestamosPDF();
  }

}
