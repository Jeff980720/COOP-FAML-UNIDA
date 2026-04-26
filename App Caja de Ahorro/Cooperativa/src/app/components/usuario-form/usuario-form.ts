import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators, ReactiveFormsModule } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA, MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { CommonModule } from '@angular/common';
import { MatIconModule } from '@angular/material/icon';
import { SociosService } from '../../services/socios';

@Component({
  selector: 'app-usuario-form',
  imports: [CommonModule, ReactiveFormsModule, MatDialogModule,
    MatFormFieldModule, MatInputModule, MatSelectModule, MatButtonModule, MatIconModule],
  templateUrl: './usuario-form.html',
  styleUrl: './usuario-form.css',
})
export class UsuarioForm implements OnInit {
  userForm: FormGroup;
  titulo: string = 'Nuevo Usuario';

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<UsuarioForm>,
    @Inject(MAT_DIALOG_DATA) public data: any // Recibe datos si es edición
  ) {
    // Definición del formulario
    this.userForm = this.fb.group({
      username: ['', Validators.required],
      // Si hay data (es edición), el password no es obligatorio
      password: ['', this.data ? [] : [Validators.required]],
      nombre_completo: ['', Validators.required],
      rol: ['USUARIO1', Validators.required],
      estado: [true]
    });
  }

  ngOnInit(): void {
    if (this.data) {
      this.titulo = 'Editar Usuario';
      // Mapeamos los datos al formulario
      this.userForm.patchValue({
        username: this.data.username,
        nombre_completo: this.data.nombre_completo,
        rol: this.data.rol,
        estado: this.data.estado
      });
    }
  }

  guardar() {
    if (this.userForm.invalid) {
      this.userForm.markAllAsTouched();
      return;
    }

    // Enviamos los valores al componente Configuracion
    this.dialogRef.close(this.userForm.value);
  }

  cancelar() {
    this.dialogRef.close(null);
  }
}
