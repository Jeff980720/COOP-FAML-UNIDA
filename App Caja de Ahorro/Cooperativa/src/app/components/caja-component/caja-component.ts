import { Component } from '@angular/core';
import { Form, FormBuilder, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { Caja } from '../../models/Caja';
import { ActivatedRoute, Router } from '@angular/router';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatButtonModule } from '@angular/material/button';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { SociosService } from '../../services/socios';
import { NgIf } from "../../../../node_modules/@angular/common/types/_common_module-chunk";

@Component({
  selector: 'app-caja-component',
  imports: [
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatAutocompleteModule,
    MatButtonModule,
    MatDatepickerModule,
    MatNativeDateModule,
    FormsModule,
  ],
  templateUrl: './caja-component.html',
  styleUrl: './caja-component.css',
})
export class CajaComponent {
  addCajaForm: FormGroup;
  caja!: Caja;
  esEdicion: boolean = false;
  listaCaja: any[] = [];

  constructor(private router: Router, private fb: FormBuilder, private sociosService: SociosService, private route: ActivatedRoute) {
    this.addCajaForm = this.fb.group({
      idCaja: ['', [Validators.required]],
      efectivo: ['', [Validators.required, Validators.pattern(/^-?(?!0\d*$)\d+(\.\d+)?$/)]],
      fechaCaja: [new Date(), [Validators.required]],
      comentario: ['', []],
    });
  }

  ngOnInit() {
    const id = this.route.snapshot.paramMap.get('id'); // O 'idCaja' según tu ruta

    // 2. Escuchar si viene un monto desde el Arqueo
    this.route.queryParams.subscribe(params => {
      if (params['monto']) {
        const montoCapturado = Number(params['monto']);

        // Llenamos el campo 'efectivo' del formulario automáticamente
        this.addCajaForm.patchValue({
          efectivo: montoCapturado
        });

        console.log('Monto cargado desde el arqueo:', montoCapturado);
      }
    });

    if (id) {
      this.esEdicion = true;
      this.cargarDatosParaEdicion(id); // Trae el 1
    } else {
      this.esEdicion = false;
      this.obtenerNuevoId(); // Trae el 2 (siguiente)
    }
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

  onSubmit() {
    // 1. Extraer la fecha del formulario
    const rawDate = this.addCajaForm.value.fechaCaja;
    // 2. Convertir a string "YYYY-MM-DD" ignorando horas
    const fechaLimpia = this.formatDate(rawDate);

    if (this.addCajaForm.valid) {
      // 2. Construye el objeto Caja tipado (Caja)
      const formValues = this.addCajaForm.getRawValue();

      const nuevoCaja: Caja = {
        idcaja: formValues.idCaja,
        efectivo: formValues.efectivo,
        fechacaja: fechaLimpia,
        comentario: formValues.comentario,
      };

      console.log('Objeto Caja tipado listo para enviar:', nuevoCaja);
      // Lógica para decidir entre CREAR o EDITAR
      if (this.esEdicion) {
        // LLAMADA PARA EDITAR
        this.sociosService.actualizarCaja(nuevoCaja.idcaja, nuevoCaja).subscribe({
          next: (res) => {
            alert('Caja actualizada correctamente.');
            this.router.navigate(['/listacaja']); // Redirección tras éxito
          },
          error: (err) => {
            console.error('Error al actualizar:', err);
            const msg = err.error?.error || 'Error al actualizar la caja.';
            alert(msg);
          }
        });
      } else {
        // LLAMADA PARA NUEVO REGISTRO
        this.sociosService.registrarCaja(nuevoCaja).subscribe({
          next: (res) => {
            alert('Caja registrada con éxito.');
            this.addCajaForm.reset();
            this.obtenerNuevoId();
          },
          error: (err) => {
            console.error('Error al registrar:', err);

            // 1. Extraemos el mensaje que viene del backend
            // 'err.error' es el objeto JSON que enviaste con res.status(400).json({ error: '...' })
            const errorMessage = err.error?.error || 'Error al guardar la nueva caja.';

            // 2. Mostramos el mensaje dinámico
            alert(errorMessage);
          }
        });
      }

    }
  }

  cargarDatosParaEdicion(id: any) {
    this.sociosService.getCajaById(id).subscribe({
      next: (data) => {
        console.log('Datos que vienen del servicio:', data); // Para verificar qué nombres trae el objeto 'data'

        // 1. Buscamos el objeto caja en la lista cargada para que el autocomplete lo reconozca
        const cajaSeleccionado = this.listaCaja.find(c => c.idcaja == data.idcaja);
        // Cuando recibes los datos del servidor para llenar el formulario:
        const fechaDeBaseDatos = data.fechacaja; // Viene como "2026-04-20" o "2026-04-20T00:00:00.000Z"

        // Forzamos a que cree la fecha sin desfase
        const dateParts = fechaDeBaseDatos.split('T')[0].split('-');
        const fechaParaElForm = new Date(
          Number(dateParts[0]),
          Number(dateParts[1]) - 1,
          Number(dateParts[2])
        );
        this.addCajaForm.patchValue({
          idCaja: data.idcaja,         // Antes tenías 'idsocio: data.idsocio'
          efectivo: data.efectivo, // Antes tenías 'nombresocio: data.nombresocio'
          // fechaCaja: new Date(data.fechacaja), // Asegúrate de convertir la fecha si viene como string
          fechaCaja: fechaParaElForm,
          comentario: data.comentario
        });

        this.esEdicion = true;
      },
      error: (err) => console.error('Error al cargar socio:', err)
    });
  }

  irASocio(): void {
    this.router.navigate(['/estado-resultados']);
  }

  irListaCaja(): void {
    this.router.navigate(['/listacaja']);
  }

  limpiarCampos(): void {
    // 1. Usa el método reset() en el FormGroup
    // this.addCajaForm.reset();
    this.addCajaForm.reset({
      idCaja: '',
      efectivo: '',
      fechaCaja: new Date(), // Mantiene la fecha actual por defecto
      comentario: ''
    });
    this.obtenerNuevoId(); // Para obtener un nuevo ID después de limpiar

  }

  // Función auxiliar para no repetir código
  obtenerNuevoId() {
    this.sociosService.getSiguienteIdCaja().subscribe(res => {
      this.addCajaForm.patchValue({
        idCaja: res.siguienteId
      });
    });
  }

}
