import { Component, OnInit } from '@angular/core';
import { SociosService } from '../../services/socios';
import { ChartConfiguration, ChartData } from 'chart.js';
import { BaseChartDirective } from 'ng2-charts';
import { CommonModule } from '@angular/common';
import { Coperativa } from '../../models/Coperativa';
import { Socio } from '../../models/Socio';
import { MatAutocompleteModule, MatAutocompleteSelectedEvent } from '@angular/material/autocomplete';
import { FormBuilder, FormGroup, ReactiveFormsModule } from '@angular/forms';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { map, startWith } from 'rxjs/operators'; // <-- IMPORTANTE: Faltaban estos
import { MatIconModule } from '@angular/material/icon';
import ChartDataLabels from 'chartjs-plugin-datalabels';
import { Chart } from 'chart.js';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
Chart.register(ChartDataLabels);

@Component({
  selector: 'app-dashboard',
  standalone: true,
  imports: [
    CommonModule,
    BaseChartDirective,
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
  templateUrl: './dashboard.html',
  styleUrls: ['./dashboard.css']
})
export class Dashboard implements OnInit {

  dashboardForm!: FormGroup; // Usar el nombre correcto consistentemente
  filteredSocios!: Observable<Socio[]>;
  listaSocios: any[] = [];
  sociosDisponibles: Socio[] = [];

  public deudaChartData: any; // <--- Agrega esto para quitar el error
  // No necesitas importar ninguna interfaz nueva
  prestamosDataSource = new MatTableDataSource<any>([]);
  // public displayedColumns: string[] = ['fecha', 'cuotas', 'capital', 'interes'];


  totalPatrimonio = 0;
  totalPrestado = 0;
  totalGanado = 0;

  public pieChartOptions: ChartConfiguration['options'] = {
    responsive: true,
    maintainAspectRatio: false, // <--- IMPORTANTE: Permite que el gráfico use todo el alto del CSS
    plugins: {
      legend: {
        display: true,
        position: 'top',
        labels: { color: '#333' } // Cambiado a oscuro para legibilidad si el fondo es claro
      }
    }
  };

  // Gráfico 1 (Ya lo tienes)
  public pieChartData: ChartData<'pie', number[], string | string[]> = {
    labels: ['Cobrado', 'Pendiente'],
    datasets: [{ data: [0, 0], backgroundColor: ['#2ecc71', '#e67e22'] }]
  };

  // AGREGA ESTO: Gráfico 2 para Rendimientos
  public pieChartData2: ChartData<'pie', number[], string | string[]> = {
    labels: ['Cobrado', 'Pendiente', 'Anulado'], // Etiquetas para los 3 datos que calculas
    datasets: [{
      data: [0, 0, 0],
      backgroundColor: ['#2ecc71', '#e67e22', '#e74c3c'] // Verde, Naranja, Rojo
    }]
  };

  // Gráfico 1: Capital (Prestado vs Cobrado vs Pendiente)
  public capitalChartData: ChartData<'bar'> = {
    labels: ['Capital'],
    datasets: [
      { data: [0], label: 'Prestado', backgroundColor: '#e67e22' },
      { data: [0], label: 'Cobrado', backgroundColor: '#2ecc71' },
      { data: [0], label: 'Pendiente', backgroundColor: '#f1c40f' }
    ]
  };

  public capitalChartOptions: ChartConfiguration['options'] = {
    responsive: true,
    plugins: {
      legend: { display: false }, // 🔴 Oculta la leyenda
      datalabels: {
        anchor: 'end',
        align: 'top',
        color: '#fff',
        font: { weight: 'bold' },
        formatter: (value) => `$${value.toLocaleString()}`
      }
    },
    scales: {
      x: {
        ticks: {
          color: '#fff', // color de las etiquetas
          font: { weight: 'bold' }
        }
      },
      y: {
        ticks: {
          color: '#fff'
        }
      }
    }
  };


  // Gráfico 2: Intereses (Cobrado vs Pendiente vs Anulado)
  public interesChartData: ChartData<'pie'> = {
    labels: ['Cobrado', 'Pendiente', 'Anulado'],
    datasets: [
      { data: [0, 0, 0], backgroundColor: ['#2ecc71', '#e67e22', '#e74c3c'] }
    ]
  };

  public interesChartOptions: ChartConfiguration['options'] = {
    responsive: true,
    plugins: { legend: { position: 'bottom' } }
  };

  // Gráfico 3: Evolución de Aportes/Pagos (Line Chart)
  public aportesChartData: ChartData<'line'> = {
    labels: ['Aportes', 'Pagos'], // puedes reemplazar con meses si tienes fechas
    datasets: [
      { data: [0], label: 'Nº Aportes', borderColor: '#3498db', fill: false },
      { data: [0], label: 'Pagos', borderColor: '#2ecc71', fill: false }
    ]
  };

  public aportesChartOptions: ChartConfiguration['options'] = {
    responsive: true,
    plugins: { legend: { position: 'bottom' } }
  };


  totalCobrar: any;
  totalCapitalPrestado: any;
  totalAyudas: any;
  totalCapitalPendiente: any;
  totalInteresPrestado: any;
  totalInteresAnulado: any;
  totalInteresPendiente: any;
  totalIngresosVarios: any;
  totalCapitalCobrado: any;
  totalInteresCobrado: any;
  totalEgresosVarios: any;
  totalUtilidad: any;
  totalAportes: any;
  totalCapital: any;
  numPrestamosSocio: any;
  numPagosSocio: any;


  constructor(
    private sociosService: SociosService,
    private fb: FormBuilder,
  ) { }

  ngOnInit(): void {
    this.dashboardForm = this.fb.group({
      idSocio: [null],
      nombreSocio: [''],
    });
    // Escuchar cambios en el input de Socio
    this.dashboardForm.get('idSocio')?.valueChanges.subscribe(value => {
      // Si el valor es null, undefined o un string vacío, volvemos a los datos globales
      if (value === null || value === undefined || value === '') {
        this.cargarDatos();
      }
    });

    // Carga inicial (Global)
    this.cargarDatos();

  }

  cargarDatos() {
    this.sociosService.getSocios().subscribe({
      next: (data: any) => {
        console.log("Respuesta bruta del servidor:", data); // <--- MIRA ESTO EN CONSOLA

        // Si el backend envía el array directo, 'data' ya es la lista.
        // Si envía el objeto, usamos 'data.items'.
        const socios = Array.isArray(data) ? data : (data.items || []);

        this.listaSocios = socios;
        this.sociosDisponibles = socios;

        this.configurarFiltroSocios();
        this.calcularKpis();

        // --- CARGA INICIAL DE TODOS LOS PRÉSTAMOS ---
        this.cargarPrestamosGlobales();

        this.sociosService.getResumenPorSocio().subscribe(resumen => {
          this.numPrestamosSocio = resumen.num_prestamos;
          this.numPagosSocio = resumen.num_pagos;
        });
      },
      error: (err) => console.error('Error en el componente:', err)
    });
  }

  // Al cargar los datos del socio:
  actualizarGraficoDeuda(prestamos: any[]) {
    const totalCapital = prestamos.reduce((acc, p) => acc + p.capitalPendiente, 0);
    const totalInteres = prestamos.reduce((acc, p) => acc + p.interesPendiente, 0);

    // Ahora esto ya no dará error porque la declaramos en el paso 1
    this.deudaChartData = {
      labels: ['Capital por Pagar', 'Interés por Pagar'],
      datasets: [{
        data: [totalCapital, totalInteres],
        backgroundColor: ['#3b82f6', '#fbbf24'],
        hoverBackgroundColor: ['#60a5fa', '#fcd34d']
      }]
    };
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

  // displaySocioFn(socio: any): string {
  //   if (!socio) return '';
  //   return socio.nombresocio ? socio.nombresocio : '';
  // }
  displaySocioFn(socio: any): string {
    // Cuando seleccionas, 'socio' es el objeto completo. 
    // Esta función extrae el string que se verá en el input.
    return socio && socio.nombresocio ? socio.nombresocio : '';
  }

  displayedColumns: string[] = [
    'socio',
    'idPrestamo',
    'capitalPendiente',
    'interesPendiente',
    'letrasPendientes',
    'totalPendiente'
  ];


  cargarPrestamosGlobales() {
    this.sociosService.getPrestamosPendientePorSocio().subscribe({
      next: (data) => {
        this.prestamosDataSource.data = data;
      },
      error: (err) => console.error('Error carga inicial:', err)
    });
  }

  onSocioSelected(event: MatAutocompleteSelectedEvent): void {
    const socioSeleccionado: Socio = event.option.value;
    const idSocio = socioSeleccionado.idsocio;

    this.dashboardForm.patchValue({
      idSocio: socioSeleccionado,
      nombreSocio: socioSeleccionado.nombresocio
    });

    this.calcularKpisSocio(socioSeleccionado);

    // --- FILTRAR TABLA POR SOCIO ---
    this.sociosService.getPrestamosPendientePorSocio(idSocio).subscribe({
      next: (data: any[]) => {
        this.prestamosDataSource.data = data; // Ahora solo mostrará los del socio
      },
      error: (err) => console.error('Error filtrando:', err)
    });

    // --- NUEVO: traer conteo de préstamos/pagos por socio ---
    this.sociosService.getResumenPorSocio(idSocio).subscribe({
      next: (resumen) => {
        this.numPrestamosSocio = resumen.num_prestamos;
        this.numPagosSocio = resumen.num_pagos;
      },
      error: (err) => console.error('Error resumen socio:', err)
    });

    this.sociosService.getResumenPorSocio(idSocio).subscribe(resumen => {
      this.numPrestamosSocio = resumen.num_prestamos;
      this.numPagosSocio = resumen.num_pagos;
    });
  }

  calcularKpisSocio(socio: Socio) {
    // KPIs individuales
    this.totalPatrimonio = Number(socio.total || 0);
    this.totalCapital = Number(socio.aportado || 0);
    this.totalAyudas = Number(socio.ayuda || 0);
    this.totalCapitalPrestado = Number(socio.montoprestado || 0);
    this.totalCapitalCobrado = Number(socio.montopagado || 0);
    this.totalCapitalPendiente = Number(socio.montopendiente || 0);
    this.totalInteresPrestado = Number(socio.interesprestado || 0);
    this.totalInteresCobrado = Number(socio.interespagado || 0);
    this.totalInteresPendiente = Number(socio.interespendiente || 0);
    this.totalInteresAnulado = Number(socio.interesanulado || 0);
    this.totalIngresosVarios = Number(socio.ingresos || 0);
    this.totalEgresosVarios = Number(socio.gastos || 0);
    this.totalUtilidad = Number(socio.ganado || 0);
    this.totalAportes = Number(socio.num_aportes || 0);

    // Gráfico 1: Cartera (Cobrado vs Pendiente)
    this.pieChartData = {
      ...this.pieChartData,
      datasets: [{
        ...this.pieChartData.datasets[0],
        data: [this.totalCapitalCobrado, this.totalCapitalPendiente]
      }]
    };

    // Gráfico 2: Rendimientos (Intereses)
    this.pieChartData2 = {
      ...this.pieChartData2,
      datasets: [{
        ...this.pieChartData2.datasets[0],
        data: [this.totalInteresCobrado, this.totalInteresPendiente, this.totalInteresAnulado]
      }]
    };

    // Gráfico 3: Capital + Intereses
    this.capitalChartData = {
      labels: ['Prestado', 'Cobrado', 'Pendiente'],
      datasets: [{
        label: 'Capital + Intereses',
        backgroundColor: ['#e67e22', '#2ecc71', '#f1c40f'],
        data: [
          this.totalCapitalPrestado + this.totalInteresPrestado,
          this.totalCapitalCobrado + this.totalInteresCobrado,
          this.totalCapitalPendiente + this.totalInteresPendiente
        ]
      }]
    };

    // Gráfico 4: Evolución de Aportes y Pagos
    this.aportesChartData = {
      labels: ['Aportes', 'Pagos'],
      datasets: [
        { label: 'Nº Aportes', data: [this.totalAportes], borderColor: '#3498db', fill: false },
        { label: 'Pagos', data: [this.totalCapitalCobrado], borderColor: '#2ecc71', fill: false }
      ]
    };
  }


  calcularKpis() {
    // Aseguramos que los valores sean numéricos para evitar el [object Object] o NaN
    this.totalPatrimonio = this.listaSocios.reduce((acc, s) => acc + Number(s.total || 0), 0);
    this.totalCapital = this.listaSocios.reduce((acc, s) => acc + Number(s.aportado || 0), 0);
    this.totalAyudas = this.listaSocios.reduce((acc, s) => acc + Number(s.ayuda || 0), 0);
    this.totalCapitalPrestado = this.listaSocios.reduce((acc, s) => acc + Number(s.montoprestado || 0), 0);
    this.totalCapitalCobrado = this.listaSocios.reduce((acc, s) => acc + Number(s.montopagado || 0), 0);
    this.totalCapitalPendiente = this.listaSocios.reduce((acc, s) => acc + Number(s.montopendiente || 0), 0);
    this.totalInteresPrestado = this.listaSocios.reduce((acc, s) => acc + Number(s.interesprestado || 0), 0);
    this.totalInteresCobrado = this.listaSocios.reduce((acc, s) => acc + Number(s.interespagado || 0), 0);
    this.totalInteresAnulado = this.listaSocios.reduce((acc, s) => acc + Number(s.interesanulado || 0), 0);
    this.totalInteresPendiente = this.listaSocios.reduce((acc, s) => acc + Number(s.interespendiente || 0), 0);
    this.totalIngresosVarios = this.listaSocios.reduce((acc, s) => acc + Number(s.ingresos || 0), 0);
    this.totalEgresosVarios = this.listaSocios.reduce((acc, s) => acc + Number(s.gastos || 0), 0);
    this.totalUtilidad = this.listaSocios.reduce((acc, s) => acc + Number(s.ganado || 0), 0);
    this.totalAportes = this.listaSocios.reduce((acc, s) => acc + Number(s.num_aportes || 0), 0);


    const pagado = this.listaSocios.reduce((acc, s) => acc + Number(s.montopagado || 0), 0);

    // Actualizar el gráfico de forma inmutable para que ng2-charts detecte el cambio
    this.pieChartData = {
      ...this.pieChartData,
      datasets: [{
        ...this.pieChartData.datasets[0],
        data: [pagado, this.totalCapitalPendiente]
      }]
    };

    const pagado2 = this.listaSocios.reduce((acc, s) => acc + Number(s.interespagado || 0), 0);

    this.pieChartData2 = {
      ...this.pieChartData2,
      datasets: [{
        ...this.pieChartData2.datasets[0],
        // CORRECCIÓN: Usa comas para pasar los valores individuales al gráfico
        data: [
          pagado2,
          this.totalInteresPendiente,
          this.totalInteresAnulado
        ]
      }]
    };

    // // Actualizar gráfico Capital
    // this.capitalChartData = {
    //   ...this.capitalChartData,
    //   datasets: [
    //     { ...this.capitalChartData.datasets[0], data: [this.totalCapitalPrestado] },
    //     { ...this.capitalChartData.datasets[1], data: [this.totalCapitalCobrado] },
    //     { ...this.capitalChartData.datasets[2], data: [this.totalCapitalPendiente] }
    //   ]
    // };
    // Totales generales combinando capital + intereses
    const totalGeneralPrestado = this.totalCapitalPrestado + this.totalInteresPrestado;
    const totalGeneralCobrado = this.totalCapitalCobrado + this.totalInteresCobrado + this.totalInteresAnulado;
    const totalGeneralPendiente = this.totalCapitalPendiente + this.totalInteresPendiente;

    // Actualizar gráfico Capital + Intereses
    // this.capitalChartData = {
    //   ...this.capitalChartData,
    //   labels: ['Capital + Intereses'],
    //   datasets: [
    //     { ...this.capitalChartData.datasets[0], data: [totalGeneralPrestado], label: 'Prestado' },
    //     { ...this.capitalChartData.datasets[1], data: [totalGeneralCobrado], label: 'Cobrado' },
    //     { ...this.capitalChartData.datasets[2], data: [totalGeneralPendiente], label: 'Pendiente' }
    //   ]
    // };
    this.capitalChartData = {
      labels: ['Prestado', 'Cobrado', 'Pendiente'], // 🔴 cada categoría como etiqueta en el eje X
      datasets: [
        {
          label: 'Capital + Intereses',
          backgroundColor: ['#e67e22', '#2ecc71', '#f1c40f'],
          data: [totalGeneralPrestado, totalGeneralCobrado, totalGeneralPendiente]
        }
      ]
    };


    // Actualizar gráfico Intereses
    this.interesChartData = {
      ...this.interesChartData,
      datasets: [{
        ...this.interesChartData.datasets[0],
        data: [this.totalInteresCobrado, this.totalInteresPendiente, this.totalInteresAnulado]
      }]
    };

    // Actualizar gráfico Aportes/Pagos
    this.aportesChartData = {
      ...this.aportesChartData,
      datasets: [
        { ...this.aportesChartData.datasets[0], data: [this.totalAportes] },
        { ...this.aportesChartData.datasets[1], data: [this.totalCapitalCobrado] }
      ]
    };
  }
}
