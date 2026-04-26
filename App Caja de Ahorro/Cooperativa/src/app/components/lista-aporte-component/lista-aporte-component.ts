import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common'; // ¡IMPORTANTE para *ngFor y pipes!
import { Aporte } from '../../models/Aportes';
import { SociosService } from '../../services/socios';
import { Router } from '@angular/router';
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

@Component({
  selector: 'app-lista-aporte-component',
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
  ], // Asegúrate de importar CommonModule para usar *ngFor
  templateUrl: './lista-aporte-component.html',
  styleUrl: './lista-aporte-component.css',
  providers: [EstadoResultado] // Lo agregas como proveedor aquí
})
export class ListaAporteComponent implements OnInit {
  aport: Aporte[] = [];

  public listaAportes: any[] = [];
  dashboardForm!: FormGroup; // Usar el nombre correcto consistentemente
  filteredSocios!: Observable<Socio[]>;
  listaSocios: any[] = [];
  sociosDisponibles: Socio[] = [];

  fechaInicioPeriodo1: string = "";
  // fechaInicioOperacion: string = "";
  fechaFinPeriodo1: string = "";

  constructor(private service: SociosService, private router: Router, private estadoResultados: EstadoResultado, private fb: FormBuilder) { }

  ngOnInit(): void {
    this.service.getAportes().subscribe(
      (data: any) => {
        // OPCIÓN A: Si quieres mantener la estructura de 'Coperativa'
        this.aport = data; // Asignamos directamente el arreglo de aportes
        console.log('Datos mapeados:', this.aport);
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
      fechaInicio: [this.service.fechaInicioPeriodo1],
      fechaFin: [this.service.fechaFinPeriodo1]
    });

    // 3. UNA SOLA suscripción para persistir y filtrar
    this.dashboardForm.valueChanges.subscribe(valores => {
      // Persistimos en el servicio
      this.service.fechaInicioPeriodo1 = valores.fechaInicio;
      this.service.fechaFinPeriodo1 = valores.fechaFin;

      // Lógica de limpieza: Si los campos están vacíos, recargamos datos globales
      if (!valores.idSocio && !valores.fechaInicio && !valores.fechaFin) {
        this.cargarDatos();
      } else {
        // Si hay datos (incluyendo las fechas que acabas de recuperar), filtramos
        this.filtrarAportes();
      }
    });

    // 4. EJECUCIÓN INICIAL: Esto evita que se pierda el filtro al cambiar de ruta
    // Si al entrar ya existen fechas, forzamos el primer filtrado
    if (this.service.fechaInicioPeriodo1 || this.service.fechaFinPeriodo1) {
      // Usamos un pequeño timeout para asegurar que los datos de 'prest' ya existan
      setTimeout(() => this.filtrarAportes(), 100);
    }
  }

  // En lista-aporte-component.ts

  // 1. ELIMINAR
  // eliminarAporte(idAporte: number): void {
  //   // 1. Primero pedimos la contraseña
  //   const password = prompt('Ingrese la contraseña de administrador para eliminar este registro:');

  //   // 2. Validamos la contraseña (puedes cambiar 'admin123' por la que prefieras)
  //   if (password === '1998') {

  //     // 3. Confirmación final de seguridad
  //     const confirmar = confirm(`¿Está seguro de eliminar el aporte #${idAporte}? Esta acción no se puede deshacer.`);

  //     if (confirmar) {
  //       this.service.eliminarAporte(idAporte).subscribe({
  //         next: () => {
  //           // Filtramos la lista para actualizar la vista
  //           this.aport = this.aport.filter(a => a.idaporte !== idAporte);
  //           alert('Registro eliminado correctamente.');
  //         },
  //         error: (err) => {
  //           console.error('Error al eliminar:', err);
  //           alert('No se pudo eliminar el registro. Verifique la consola.');
  //         }
  //       });
  //     }
  //   } else if (password !== null) {
  //     // Si el usuario escribió algo pero no es la clave correcta
  //     alert('Contraseña incorrecta. No tienes permiso para realizar esta acción.');
  //   }
  // }

  eliminarAporte(idAporte: any): void {
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
        this.ejecutarEliminacion(idAporte);
      } else if (result.isConfirmed) {
        Swal.fire('Error', 'Contraseña incorrecta', 'error');
      }
    });
  }

  // Mueve la lógica del subscribe a una función aparte
  private ejecutarEliminacion(idAporte: any) {
    this.service.eliminarAporte(idAporte).subscribe({
      next: () => {
        this.aport = this.aport.filter(a => a.idaporte !== idAporte);
        Swal.fire('Eliminado', 'Registro borrado con éxito', 'success');
      },
      error: (err) => {
        const msg = err.error?.message || 'No se puede eliminar porque tiene pagos.';
        Swal.fire('Error', msg, 'error');
      }
    });
  }

  // 2. ACTUALIZAR
  editarAporte(aporte: any) {
    if (aporte && aporte.idaporte) {
      this.router.navigate(['/editar-aporte', aporte.idaporte])
        .then(nav => console.log('¿Navegación exitosa?:', nav))
        .catch(err => console.error('Error en navegación:', err));
    } else {
      console.error('El objeto aporte no tiene un idaporte válido', aporte);
    }
  }

  irASocio(): void {
    this.router.navigate(['/socios']);
  }

  irFormAport(): void {
    this.router.navigate(['/aportes']);
  }

  cargarDatos() {
    // Asegúrate de llenar la variable con los datos de tu servicio
    this.service.getAportes().subscribe(res => {
      this.listaAportes = res;
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

    this.filtrarAportes();
  }

  displaySocioFn(socio: any): string {
    // Cuando seleccionas, 'socio' es el objeto completo. 
    // Esta función extrae el string que se verá en el input.
    return socio && socio.nombresocio ? socio.nombresocio : '';
  }

  filtrarAportes(): void {
    // const idSocio = this.dashboardForm.get('idSocio')?.value?.idsocio;
    // const fechaInicio = this.dashboardForm.get('fechaInicio')?.value;
    // const fechaFin = this.dashboardForm.get('fechaFin')?.value;

    const { idSocio, fechaInicio, fechaFin } = this.dashboardForm.value;

    const inicioStr = fechaInicio ? new Date(fechaInicio).toISOString().split('T')[0] : undefined;
    const finStr = fechaFin ? new Date(fechaFin).toISOString().split('T')[0] : undefined;

    this.service.getAportesFiltrados(idSocio?.idsocio, inicioStr, finStr).subscribe({
      next: (data: any[]) => {
        this.listaAportes = data;
        console.log('Aportes filtrados:', data);
      },
      error: (err) => console.error('Error filtrando préstamos:', err)
    });

  }

  botonDescargarPDF() {
    // Ahora 'listaPrestamos' ya existe y la función acepta 1 argumento
    this.estadoResultados.exportarListaAportesPDF();
  }

}
