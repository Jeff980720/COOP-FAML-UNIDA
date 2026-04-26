import { Component } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Coperativa } from '../../models/Coperativa';
import { Socio } from '../../models/Socio';

import { FormBuilder, FormControl, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatButtonModule } from '@angular/material/button';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { SociosService } from '../../services/socios';

@Component({
  selector: 'app-add-socio-component',
  standalone: true,
  imports: [
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatAutocompleteModule,
    MatButtonModule,
    MatDatepickerModule,
    MatNativeDateModule,
    FormsModule
  ],
  templateUrl: './add-socio-component.html',
  styleUrl: './add-socio-component.css',
})
export class AddSocioComponent {
  addSocioForm!: FormGroup;
  socio!: Socio;
  esEdicion: boolean = false;
  listaSocios: any[] = [];

  constructor(private router: Router, private fb: FormBuilder, private sociosService: SociosService, private route: ActivatedRoute) {
  }

  ngOnInit() {
    this.addSocioForm = this.fb.group({
      // REGLA: Obligatorio y exactamente 2 dígitos
      id: ['', [Validators.required, Validators.pattern(/^\d{2}$/)]],

      // REGLA: Nombre con validaciones completas
      nombre: ['', [
        Validators.required,
        Validators.minLength(5),
        Validators.pattern(/^[a-zA-ZñÑáéíóúÁÉÍÓÚ\s]*$/)
      ]],

      // El resto de campos numéricos
      aportado: [0],
      montoprestado: [0],
      montopagado: [0],
      montopendiente: [0],
      interesprestado: [0],
      interespagado: [0],
      interesanulado: [0],
      interespendiente: [0],
    });

    const id = this.route.snapshot.paramMap.get('id');
    if (id) {
      this.esEdicion = true;
      this.cargarDatosParaEdicion(id);
    } else {
      this.obtenerNuevoId();
    }
  }

  onSubmit() {
    if (this.addSocioForm.valid) {
      // Usamos getRawValue() para capturar el 'idsocio' incluso si está deshabilitado
      const formValues = this.addSocioForm.getRawValue();

      const datosSocio: Socio = {
        idsocio: formValues.id, // Asegúrate que el nombre coincida con tu FormGroup
        nombresocio: formValues.nombre,
        aportado: formValues.aportado,
        montoprestado: formValues.montoprestado,
        montopagado: formValues.montopagado,
        montopendiente: formValues.montopendiente,
        interesprestado: formValues.interesprestado,
        interespagado: formValues.interespagado,
        interesanulado: formValues.interesanulado,
        interespendiente: formValues.interespendiente,
      };

      console.log('Objeto Socio listo para procesar:', datosSocio);

      // Lógica para decidir entre CREAR o EDITAR
      if (this.esEdicion) {
        // LLAMADA PARA EDITAR
        this.sociosService.actualizarSocio(datosSocio.idsocio, datosSocio).subscribe({
          next: (res) => {
            alert('Socio actualizado correctamente.');
            this.router.navigate(['/socios']); // Redirección tras éxito
          },
          error: (err) => {
            console.error('Error al actualizar:', err);
            alert('Error al actualizar el socio.');
          }
        });
      } else {
        // LLAMADA PARA NUEVO REGISTRO
        this.sociosService.registrarSocio(datosSocio).subscribe({
          next: (res) => {
            alert('Socio registrado con éxito.');
            this.addSocioForm.reset();
            this.obtenerNuevoId(); // Genera el ID para el siguiente registro
          },
          error: (err) => {
            console.error('Error al registrar:', err);
            alert('Error al guardar el nuevo socio.');
          }
        });
      }

    } else {
      console.log('Formulario inválido.');
      this.addSocioForm.markAllAsTouched(); // Muestra errores visuales al usuario
    }
  }

  cargarDatosParaEdicion(id: string) {
    this.sociosService.getSocioById(id).subscribe({
      next: (data) => {
        console.log('Datos que vienen del servicio:', data); // Para verificar qué nombres trae el objeto 'data'

        this.addSocioForm.patchValue({
          id: data.idsocio,         // Antes tenías 'idsocio: data.idsocio'
          nombre: data.nombresocio, // Antes tenías 'nombresocio: data.nombresocio'
          aportado: data.aportado,
          montoprestado: data.montoprestado,
          montopagado: data.montopagado,
          montopendiente: data.montopendiente,
          interesprestado: data.interesprestado,
          interespagado: data.interespagado,
          interespendiente: data.interespendiente,
          // Si tienes 'interesanulado' en el backend pero no en el fb.group, Angular lo ignorará.
        });

        this.esEdicion = true;
      },
      error: (err) => console.error('Error al cargar socio:', err)
    });
  }

  convertirAMayusculas() {
    const nombreControl = this.addSocioForm.get('nombre');
    if (nombreControl && nombreControl.value) {
      // Convierte el valor actual a mayúsculas y lo actualiza en el formulario
      nombreControl.setValue(nombreControl.value.toUpperCase(), { emitEvent: false });
    }
  }

  irASocio(): void {
    this.router.navigate(['/socios']);
  }

  // Función auxiliar para no repetir código
  obtenerNuevoId() {
    this.sociosService.getSiguienteIdSocio().subscribe(res => {
      this.addSocioForm.patchValue({
        id: res.siguienteId
      });
    });
  }

  limpiarCampos(): void {
    // 1. Usa el método reset() en el FormGroup
    this.addSocioForm.reset({
      id: '', // O podrías llamar a obtenerNuevoId() para asignar el siguiente ID automáticamente
      nombre: '',
      aportado: 0,
      montoprestado: 0,
      montopagado: 0,
      montopendiente: 0,
      interesprestado: 0,
      interespagado: 0,
      interespendiente: 0,
      interesanulado: 0,

    });
    this.obtenerNuevoId(); // Para obtener un nuevo ID después de limpiar
    // 2. Opcional: Re-aplicar valores iniciales si son dinámicos (como la fecha)
    // El método reset() normalmente revierte a los valores dados en el constructor.
    // Si quieres asegurar que la fecha vuelva a ser la de hoy:
    // this.addSocioForm.patchValue({
    //   fechaAporte: new Date(),
    //   aportado: 0
    // });
  }
}
