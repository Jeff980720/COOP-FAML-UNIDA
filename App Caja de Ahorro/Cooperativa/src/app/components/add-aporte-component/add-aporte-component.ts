import { Component, OnInit } from '@angular/core';
import { Socio } from '../../models/Socio';
import { FormControl, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { SociosService } from '../../services/socios';
import { map, Observable, startWith } from 'rxjs';
import { CommonModule } from '@angular/common';
import { Aporte } from '../../models/Aportes';
import { FormBuilder } from '@angular/forms';

// 💡 ¡AÑADE ESTAS IMPORTACIONES FALTANTES! 💡
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatAutocompleteModule, MatAutocompleteSelectedEvent } from '@angular/material/autocomplete';
import { MatButtonModule } from '@angular/material/button';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { ActivatedRoute, Router } from '@angular/router';
// Fin de importaciones de Material

@Component({
  selector: 'app-add-aporte-component',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatAutocompleteModule,
    MatButtonModule,
    MatDatepickerModule,
    MatNativeDateModule,
  ],
  templateUrl: './add-aporte-component.html',
  styleUrl: './add-aporte-component.css',
})
export class AddAporteComponent implements OnInit {
  // 💡 Propiedad inicializada en el constructor (solución TS2564)
  aporteForm: FormGroup;
  sociosDisponibles: Socio[] = [];
  filteredSocios!: Observable<Socio[]>;
  esEdicion: boolean = false;
  vacio: boolean = true;
  idEdicion: string | null = null;


  constructor(private sociosService: SociosService, private router: Router, private fb: FormBuilder, private route: ActivatedRoute) {
    // 💡 Inicialización en el constructor
    this.aporteForm = new FormGroup({
      // Puedes usar 'string' para numeroAporte si es un código
      numeroAporte: new FormControl('', Validators.required),
      fechaAporte: new FormControl(new Date(), Validators.required),
      // Este FormControl va a contener el objeto Socio completo después de la selección
      idSocio: new FormControl(null, Validators.required),
      // Campo que se llena automáticamente y está deshabilitado
      nombreSocio: new FormControl({ value: '', disabled: false }),
      // nombreSocio: new FormControl(''),
      aportado: new FormControl(0, [Validators.required, Validators.min(0.01)]),
      comentario: new FormControl(''),
    });
  }

  irASocio(): void {
    this.router.navigate(['/socios']);
  }

  irListaAport(): void {
    this.router.navigate(['/listaportes']);
  }

  limpiarCampos(): void {
    // 1. Usa el método reset() en el FormGroup
    this.aporteForm.reset({
      numeroAporte: '',
      fechaAporte: new Date(), // Mantiene la fecha actual por defecto
      idSocio: null,
      nombreSocio: '',
      aportado: 0,
      comentario: ''
    });
    this.obtenerNuevoId(); // Para obtener un nuevo ID después de limpiar
  }


  ngOnInit() {
    // 1. Inicializar el formulario con valores por defecto
    this.aporteForm = this.fb.group({
      numeroAporte: ['', Validators.required], // Puedes usar 'string' si es un código alfanumérico
      fechaAporte: [new Date(), Validators.required],
      idSocio: [null, Validators.required],
      nombreSocio: ['', Validators.required],
      aportado: [0, [Validators.required, Validators.min(1)]],
      comentario: [''],
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
          this.cargarDatosAporte(+id); // Convertimos el ID de la URL a número
        } else {
          this.esEdicion = false;
          this.obtenerNuevoId();
        }
      },
      error: (err) => console.error('Error al cargar socios:', err)
    });
  }

  // Dentro de export class AddAporteComponent ...

  configurarFiltroSocios() {
    this.filteredSocios = this.aporteForm.get('idSocio')!.valueChanges.pipe(
      startWith(''),
      map(value => {
        const name = typeof value === 'string' ? value : value?.nombre;
        return name ? this._filter(name) : this.sociosDisponibles.slice();
      })
    );
  }

  private _filter(value: string): Socio[] {
    const filterValue = value.toLowerCase();
    return this.sociosDisponibles.filter(socio =>
      socio.nombresocio.toLowerCase().includes(filterValue) ||
      // Usamos .idsocio porque así está en Socio.ts
      socio.idsocio.toString().includes(filterValue)
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
    this.aporteForm.get('nombreSocio')!.setValue(socioSeleccionado.nombresocio);
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

  registrarAporte() {

    if (this.aporteForm.invalid) return; //
    // 1. Extraer la fecha del formulario
    const rawDate = this.aporteForm.value.fechaAporte;

    // 2. Convertir a string "YYYY-MM-DD" ignorando horas
    const fechaLimpia = this.formatDate(rawDate);

    // 1. Obtenemos todos los valores (incluyendo los deshabilitados como numeroAporte)
    const formValues = this.aporteForm.getRawValue();

    // 2. EXTRAER EL ID: Si es un objeto (del autocomplete), tomamos .id; si no, el valor directo
    // Usamos .id porque así está definido en tu modelo Socio
    const valorSocio = formValues.idSocio;
    const idSocioFinal = valorSocio && typeof valorSocio === 'object'
      ? valorSocio.idsocio
      : valorSocio;

    // 3. Estructurar el objeto exactamente como lo espera tu tabla SQL
    const datosAEnviar = {
      idaporte: Number(formValues.numeroAporte),
      fechaaporte: fechaLimpia,
      aportado: formValues.aportado,
      idsocio: String(idSocioFinal), // Convertimos a string para evitar errores en VARCHAR(50)
      nombresocio: formValues.nombreSocio,
      comentario: formValues.comentario
    };

    if (this.esEdicion) {
      // --- LÓGICA DE ACTUALIZACIÓN (PUT) ---
      const idParam = this.route.snapshot.paramMap.get('id');
      if (idParam) {
        this.sociosService.actualizarAporte(+idParam, datosAEnviar).subscribe({
          next: (res) => {
            alert('Aporte actualizado correctamente');
            this.router.navigate(['/listaportes']);
          },
          error: (err) => {
            console.error('Error al actualizar:', err);
            alert('Error al actualizar el aporte.');
          }
        });
      }
    } else {
      // --- LÓGICA DE CREACIÓN (POST) ---
      this.sociosService.registrarAporte(datosAEnviar as any).subscribe({
        next: (res) => {
          alert('Aporte guardado correctamente');
          this.limpiarYPrepararNuevo();
        },
        error: (err) => {
          console.error('Error al guardar:', err);
          // Esto previene el error 500 al enviar datos limpios
          alert('No se pudo guardar el aporte. Revise que el ID no esté duplicado.');
        }
      });
    }
  }

  // Función auxiliar para mantener limpio el código
  limpiarYPrepararNuevo() {
    // Si estamos editando, ignoramos la orden de limpiar
    if (this.esEdicion) {
      return;
    }

    this.aporteForm.reset({
      fechaAporte: new Date(),
      aportado: 0
    });
    this.obtenerNuevoId(); //
  }

  // Función auxiliar para no repetir código
  obtenerNuevoId() {
    this.sociosService.getSiguienteIdAporte().subscribe(res => {
      this.aporteForm.patchValue({
        numeroAporte: res.siguienteId
      });
    });
  }

  cargarDatosAporte(id: number) {
    this.sociosService.getAporteById(id).subscribe({
      next: (res) => {
        if (res) {
          // CAMBIO CRÍTICO: Usar s.id para que coincida con tu modelo
          const socioCompleto = this.sociosDisponibles.find(s =>
            String(s.idsocio) === String(res.idsocio)
          );

          // Cuando recibes los datos del servidor para llenar el formulario:
          const fechaDeBaseDatos = res.fechaaporte; // Viene como "2026-04-20" o "2026-04-20T00:00:00.000Z"

          // Forzamos a que cree la fecha sin desfase
          const dateParts = fechaDeBaseDatos.split('T')[0].split('-');
          const fechaParaElForm = new Date(
            Number(dateParts[0]),
            Number(dateParts[1]) - 1,
            Number(dateParts[2])
          );

          this.aporteForm.patchValue({
            numeroAporte: res.idaporte,
            fechaAporte: fechaParaElForm,
            // Pasamos el objeto completo encontrado para el autocomplete
            idSocio: socioCompleto ? socioCompleto : res.idsocio,
            nombreSocio: res.nombresocio,
            aportado: res.aportado,
            comentario: res.comentario
          });
        }
      },
      error: (err) => console.error('Error al cargar datos:', err)
    });
  }

}


