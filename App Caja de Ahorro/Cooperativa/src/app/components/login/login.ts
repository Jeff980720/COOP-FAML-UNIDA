// src/app/auth/login/login.component.ts
import { Component } from '@angular/core';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { SociosService } from '../../services/socios';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { CommonModule } from '@angular/common'; // <-- 1. Importa esto
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import Swal from 'sweetalert2'; // Opcional, para alertas bonitas

@Component({
  selector: 'app-login',
  imports: [ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatButtonModule,
    MatCardModule,  // <-- Nuevo
    MatIconModule,  // <-- Nuevo
    CommonModule],
  templateUrl: './login.html',
  styleUrl: './login.css',
})
export class Login {
  // 1. Declaramos la propiedad sin inicializarla aquí
  miFormulario!: FormGroup;
  hide = true; // Variable para controlar la visibilidad de la contraseña

  constructor(
    private fb: FormBuilder,
    private router: Router,
    private authService: SociosService
  ) {
    // 2. La inicializamos dentro del constructor
    this.miFormulario = this.fb.group({
      username: ['', [Validators.required]],
      password: ['', [Validators.required, Validators.minLength(4)]]
    });
  }

  // ingresar() {
  //   if (this.miFormulario.invalid) return;

  //   const { username, password } = this.miFormulario.value;

  //   this.authService.login(username, password).subscribe(success => {
  //     if (success) {
  //       this.router.navigate(['/socios']); // Navega solo si el backend dio OK
  //     } else {
  //       // Aquí puedes mostrar una alerta de "Usuario o clave incorrectos"
  //       console.log('Fallo el ingreso');
  //     }
  //   });
  // }
  ingresar() {
    // Si el formulario es inválido (ej. contraseña muy corta), marcamos los errores visualmente
    if (this.miFormulario.invalid) {
      this.miFormulario.markAllAsTouched();
      return;
    }

    const { username, password } = this.miFormulario.value;

    this.authService.login(username, password).subscribe({
      next: (res: any) => {
        if (res.ok) {
          this.router.navigate(['/socios']);
        }
      },
      error: (err) => {
        // Configuramos el Toast para que aparezca en la esquina superior
        const Toast = Swal.mixin({
          toast: true,
          position: 'top-end',
          showConfirmButton: false,
          timer: 3000,
          timerProgressBar: true
        });

        Toast.fire({
          icon: 'error',
          title: err.error?.msg || 'Credenciales incorrectas' // Aquí atrapas el mensaje del backend
        });
      }
    });
  }
}   
