import { Component } from '@angular/core';
import { Pagos } from '../../models/Pagos';
import { SociosService } from '../../services/socios';
import { Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { EstadoResultado } from '../estado-resultado/estado-resultado';
import { FormBuilder, FormGroup, ReactiveFormsModule } from '@angular/forms';
import { map, Observable, startWith } from 'rxjs';
import { Socio } from '../../models/Socio';
import { MatAutocompleteModule, MatAutocompleteSelectedEvent } from '@angular/material/autocomplete';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { MatTableModule } from '@angular/material/table';
import Swal from 'sweetalert2';

@Component({
  selector: 'app-lista-pagos-component',
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
  templateUrl: './lista-pagos-component.html',
  styleUrl: './lista-pagos-component.css',
  providers: [EstadoResultado] // Lo agregas como proveedor aquí
})
export class ListaPagosComponent {

  pags: Pagos[] = [];

  public listaPagos: any[] = [];
  dashboardForm!: FormGroup; // Usar el nombre correcto consistentemente
  filteredSocios!: Observable<Socio[]>;
  listaSocios: any[] = [];
  sociosDisponibles: Socio[] = [];

  fechaInicioPeriodo3: string = "";
  // fechaInicioOperacion: string = "";
  fechaFinPeriodo3: string = "";

  constructor(private service: SociosService, private router: Router, private estadoResultados: EstadoResultado, private fb: FormBuilder) { }

  // ngOnInit(): void {
  //   this.service.getPagos().subscribe(
  //     (data: any) => {
  //       // OPCIÓN A: Si quieres mantener la estructura de 'Coperativa'
  //       this.pags = data; // Asignamos directamente el arreglo de aportes
  //       console.log('Datos mapeados:', this.pags);
  //     },
  //     (error) => {
  //       console.error('Error:', error);
  //     }
  //   );
  // }

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

    this.service.getPagos().subscribe(
      (data: any) => {
        // OPCIÓN A: Si quieres mantener la estructura de 'Coperativa'
        this.pags = data; // Asignamos directamente el arreglo de aportes
        console.log('Datos mapeados:', this.pags);
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
      fechaInicio: [this.service.fechaInicioPeriodo3],
      fechaFin: [this.service.fechaFinPeriodo3]
    });

    // 3. UNA SOLA suscripción para persistir y filtrar
    this.dashboardForm.valueChanges.subscribe(valores => {
      // Persistimos en el servicio
      this.service.fechaInicioPeriodo3 = valores.fechaInicio;
      this.service.fechaFinPeriodo3 = valores.fechaFin;

      // Lógica de limpieza: Si los campos están vacíos, recargamos datos globales
      if (!valores.idSocio && !valores.fechaInicio && !valores.fechaFin) {
        this.cargarDatos();
      } else {
        // Si hay datos (incluyendo las fechas que acabas de recuperar), filtramos
        this.filtrarPagos();
      }
    });

    // 4. EJECUCIÓN INICIAL: Esto evita que se pierda el filtro al cambiar de ruta
    // Si al entrar ya existen fechas, forzamos el primer filtrado
    if (this.service.fechaInicioPeriodo3 || this.service.fechaFinPeriodo3) {
      // Usamos un pequeño timeout para asegurar que los datos de 'prest' ya existan
      setTimeout(() => this.filtrarPagos(), 100);
    }
  }

  // En lista-aporte-component.ts

  // 1. ELIMINAR
  // eliminarPagos(idPagos: any): void {
  //   const password = prompt('Ingrese la contraseña de administrador para eliminar el pago:');

  //   if (password === '1998') {
  //     const confirmar = confirm(`¿Está seguro de eliminar el registro #${idPagos}?`);

  //     if (confirmar) {
  //       this.service.eliminarPagos(idPagos).subscribe({
  //         next: (res) => {
  //           // Solo si el servidor borra realmente, actualizamos la vista
  //           this.pags = this.pags.filter(a => a.idpagos !== idPagos);
  //           alert('Registro eliminado correctamente.');
  //         },
  //         error: (err) => {
  //           // Capturamos el mensaje de error del Backend
  //           if (err.status === 400 && err.error.message) {
  //             alert(err.error.message);
  //           } else {
  //             console.error('Error detallado:', err);
  //             alert('Error al intentar eliminar. Es posible que el registro tenga dependencias activas.');
  //           }
  //           // AL NO ESTAR EN 'NEXT', LA LISTA NO SE FILTRA Y EL REGISTRO PERMANECE VISIBLE
  //         }
  //       });
  //     }
  //   } else if (password !== null) {
  //     alert('Contraseña incorrecta.');
  //   }
  // }

  eliminarPagos(idPagos: any): void {
    Swal.fire({
      title: 'Seguridad de Administrador',
      input: 'password',
      inputLabel: 'Ingrese la contraseña para eliminar:',
      showCancelButton: true,
      confirmButtonText: 'Validar',
      cancelButtonText: 'Cancelar'
    }).then((result:any) => {
      if (result.value === '1998') {
        // Si la clave es correcta, procedemos
        this.ejecutarEliminacion(idPagos);
      } else if (result.isConfirmed) {
        Swal.fire('Error', 'Contraseña incorrecta', 'error');
      }
    });
  }

  // Mueve la lógica del subscribe a una función aparte
  private ejecutarEliminacion(idPagos: any) {
    this.service.eliminarPagos(idPagos).subscribe({
      next: () => {
        this.pags = this.pags.filter(a => a.idpagos !== idPagos);
        Swal.fire('Eliminado', 'Registro borrado con éxito', 'success');
      },
      error: (err) => {
        const msg = err.error?.message || 'No se puede eliminar porque tiene pagos.';
        Swal.fire('Error', msg, 'error');
      }
    });
  }

  // 2. ACTUALIZAR
  // editarPagos(pagos: any) {
  //   const password = prompt('Ingrese la contraseña de administrador para editar el pago:');

  //   if (password === '1998') {
  //     console.log('Intentando editar el aporte con ID:', pagos.idpagos);

  //     if (pagos && pagos.idpagos) {
  //       this.router.navigate(['/editar-pagos', pagos.idpagos])
  //         .then(nav => console.log('¿Navegación exitosa?:', nav))
  //         .catch(err => console.error('Error en navegación:', err));
  //     } else {
  //       console.error('El objeto aporte no tiene un idaporte válido', pagos);
  //     }
  //   } else if (password !== null) {
  //     alert('Contraseña incorrecta.');
  //   }
  // }

  editarPagos(pagos: any) {
    // 1. Sustitución de prompt por SweetAlert2 para evitar bloqueos en escritorio
    Swal.fire({
      title: 'Seguridad de Administrador',
      text: 'Ingrese la contraseña para editar el pago:',
      input: 'password',
      inputAttributes: {
        autocapitalize: 'off'
      },
      showCancelButton: true,
      confirmButtonText: 'Validar',
      cancelButtonText: 'Cancelar',
      confirmButtonColor: '#28a745',
      cancelButtonColor: '#d33'
    }).then((result:any) => {
      if (result.isConfirmed) {
        // 2. Validación de la clave '1998'
        if (result.value === '1998') {
          console.log('Intentando editar el pago con ID:', pagos.idpagos);

          // 3. Verificación de ID y Navegación
          if (pagos && pagos.idpagos) {
            this.router.navigate(['/editar-pagos', pagos.idpagos])
              .then(nav => {
                if (nav) {
                  console.log('Navegación a edición de pagos exitosa');
                } else {
                  console.warn('La navegación falló. Revisa tus rutas.');
                }
              })
              .catch(err => console.error('Error en navegación:', err));
          } else {
            console.error('El objeto pago no tiene un idpagos válido', pagos);
            Swal.fire('Error', 'No se encontró un ID de pago válido.', 'error');
          }
        } else {
          // Alerta de contraseña incorrecta
          Swal.fire({
            icon: 'error',
            title: 'Acceso Denegado',
            text: 'Contraseña incorrecta.',
            timer: 2000,
            showConfirmButton: false
          });
        }
      }
    });
  }

  irASocio(): void {
    this.router.navigate(['/socios']);
  }

  irFormPago(): void {
    this.router.navigate(['/pagos']);
  }

  cargarDatos() {
    // Asegúrate de llenar la variable con los datos de tu servicio
    this.service.getPagos().subscribe(res => {
      this.listaPagos = res;
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

    this.filtrarPagos();
  }

  displaySocioFn(socio: any): string {
    // Cuando seleccionas, 'socio' es el objeto completo. 
    // Esta función extrae el string que se verá en el input.
    return socio && socio.nombresocio ? socio.nombresocio : '';
  }

  filtrarPagos(): void {
    // const idSocio = this.dashboardForm.get('idSocio')?.value?.idsocio;
    // const fechaInicio = this.dashboardForm.get('fechaInicio')?.value;
    // const fechaFin = this.dashboardForm.get('fechaFin')?.value;

    const { idSocio, fechaInicio, fechaFin } = this.dashboardForm.value;

    const inicioStr = fechaInicio ? new Date(fechaInicio).toISOString().split('T')[0] : undefined;
    const finStr = fechaFin ? new Date(fechaFin).toISOString().split('T')[0] : undefined;

    this.service.getPagosFiltrados(idSocio?.idsocio, inicioStr, finStr).subscribe({
      next: (data: any[]) => {
        this.listaPagos = data;
        console.log('Prestamos filtrados:', data);
      },
      error: (err) => console.error('Error filtrando préstamos:', err)
    });

  }

  botonDescargarPDF() {
    // Ahora 'listaPrestamos' ya existe y la función acepta 1 argumento
    this.estadoResultados.exportarListaPagosPDF();
  }
}
