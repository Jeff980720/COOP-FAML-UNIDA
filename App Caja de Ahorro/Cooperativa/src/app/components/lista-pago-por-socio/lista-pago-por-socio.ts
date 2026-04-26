import { Component } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { SociosService } from '../../services/socios';
import { CommonModule } from '@angular/common';
import jsPDF from 'jspdf';
import autoTable from 'jspdf-autotable';

@Component({
  selector: 'app-lista-pago-por-socio',
  imports: [CommonModule],
  templateUrl: './lista-pago-por-socio.html',
  styleUrl: './lista-pago-por-socio.css',
})
export class ListaPagoPorSocio {

  pagos: any[] = [];
  nombreSocio: string = '';
  date: Date = new Date();

  constructor(private route: ActivatedRoute, private service: SociosService, private router: Router) { }

  ngOnInit(): void {
    // Capturamos el ID que enviamos desde la tabla de socios
    const id = this.route.snapshot.paramMap.get('id');
    if (id) {
      this.cargarPrestamos(id);
    }
  }

  cargarPrestamos(id: string) {
    this.service.getPagosBySocio(id).subscribe({
      next: (data) => {
        this.pagos = data;
        if (data.length > 0) {
          // Opcional: Tomar el nombre del primer registro para el título
          this.nombreSocio = data[0].nombresocio;
        }
      },
      error: (err) => console.error('Error loading loans', err)
    });
  }

  generarPDFPagos(idSocio: string, pagos: any[]) {
    const doc = new jsPDF('l', 'mm', 'a4');
    const pageWidth = doc.internal.pageSize.getWidth();

    // --- CÁLCULO DE TOTALES DINÁMICO ---
    const totalCapital = pagos.reduce((acc, p) => {
      const numAmort = (p.numamortizacion || '').toUpperCase();
      const tipo = (p.tipo || '').toUpperCase();
      const monto = Number(p.amortizacion || 0);

      if (idSocio === '02') {
        // Para SOCIOS GASTOS: Sumar solo GST y AYUDA
        return (numAmort.includes('GST') || tipo.includes('AYUDA')) ? acc + monto : acc;
      } else {
        // Para socios normales: Sumar solo si NO es GST ni AYUDA
        return (numAmort.includes('GST') || tipo.includes('AYUDA')) ? acc : acc + monto;
      }
    }, 0);

    const totalInteres = pagos.reduce((acc, p) => {
      const numInt = (p.numinteres || '').toUpperCase();
      const tipo = (p.tipo || '').toUpperCase();
      const monto = Number(p.interes || 0);

      if (idSocio === '02') {
        // Para SOCIOS GASTOS: Sumar solo GST y AYUDA
        return (numInt.includes('GST') || numInt.includes('AYUDA') || tipo.includes('AYUDA')) ? acc + monto : acc;
      } else {
        // Para socios normales: Excluir GST y AYUDA
        return (numInt.includes('GST') || numInt.includes('AYUDA') || tipo.includes('AYUDA')) ? acc : acc + monto;
      }
    }, 0);

    // --- ENCABEZADO ---
    const azulTitulo = [26, 35, 46];
    doc.setTextColor(azulTitulo[0], azulTitulo[1], azulTitulo[2]);
    doc.setFontSize(20);
    doc.setFont('helvetica', 'bold');
    doc.text('REPORTE DETALLADO DE PAGOS', 15, 20);

    doc.setFontSize(11);
    doc.setFont('helvetica', 'normal');
    const nombre = pagos[0]?.nombresocio || 'Socio';
    doc.setTextColor(100, 100, 100);
    doc.text(`CLIENTE: ${nombre.toUpperCase()} (ID: ${idSocio})`, 15, 30);

    const fechaTexto = `FECHA DE REPORTE: ${new Date().toLocaleDateString()}`;
    doc.text(fechaTexto, pageWidth - 15, 30, { align: 'right' });

    // --- TABLA ---
    autoTable(doc, {
      startY: 35,
      margin: { left: 15, right: 15 },
      columns: [
        { header: '#', dataKey: 'index' },
        { header: 'ID PREST.', dataKey: 'idprestamos' },
        { header: 'TIPO', dataKey: 'tipo' },
        { header: 'PAGO CAPITAL', dataKey: 'amortizacion' },
        { header: 'FECHA CAP.', dataKey: 'fechaamortizacion' },
        { header: 'N° CUOTA CAP.', dataKey: 'numamortizacion' },
        { header: 'PAGO INTERÉS', dataKey: 'interes' },
        { header: 'FECHA INT.', dataKey: 'fechainteres' },
        { header: 'N° CUOTA INT.', dataKey: 'numinteres' },
        { header: 'OBSERVACIÓN', dataKey: 'comentario' }
      ],
      body: pagos.map((p, idx) => ({
        index: idx + 1,
        idprestamos: p.idprestamos || '---',
        tipo: p.tipo || 'N/A',
        amortizacion: `$${Number(p.amortizacion || 0).toFixed(2)}`,
        fechaamortizacion: p.fechaamortizacion ? new Date(p.fechaamortizacion).toLocaleDateString() : '---',
        numamortizacion: p.numamortizacion || '',
        interes: `$${Number(p.interes || 0).toFixed(2)}`,
        fechainteres: p.fechainteres ? new Date(p.fechainteres).toLocaleDateString() : '---',
        numinteres: p.numinteres || '',
        comentario: p.comentario || ''
      })),
      theme: 'grid',
      headStyles: { fillColor: [44, 62, 80], fontSize: 8 },
      styles: { fontSize: 7.5, cellPadding: 2 },
      foot: [[
        {
          content: idSocio === '02' ? 'TOTAL GASTOS:' : 'TOTAL INGRESOS:',
          colSpan: 3,
          styles: { halign: 'right', fontStyle: 'bold', textColor: [0, 0, 0] }
        },
        {
          content: `$${totalCapital.toFixed(2)}`,
          styles: { fontStyle: 'bold', fillColor: [255, 255, 255], textColor: [0, 0, 0], fontSize: 9 }
        },
        '', '',
        {
          content: `$${totalInteres.toFixed(2)}`,
          styles: { fontStyle: 'bold', fillColor: [255, 255, 255], textColor: [0, 0, 0], fontSize: 9 }
        },
        '', '', ''
      ]],
      showFoot: 'lastPage'
    });

    doc.save(`Pagos_${nombre.replace(/\s+/g, '_')}_${idSocio}.pdf`);
  }

  // Función para obtener la suma total de una propiedad específica
  // ... dentro de tu clase ListaPrestamoPorSocio

  // getTotales(propiedad: string): number {
  //   // if (!this.pagos || this.pagos.length === 0) return 0;

  //   return this.pagos.reduce((acc, obj) => {
  //     // 1. Verificamos si es un registro de tipo EGRESO
  //     // Ajusta 'tipo' al nombre real de la columna en tu base de datos
  //     const esEgreso = obj.numamortizacion && String(obj.numamortizacion).toUpperCase().startsWith('GST');
  //     const esGasto = obj.numamortizacion && String(obj.numamortizacion).toUpperCase().startsWith('AYUDA');

  //     if (!esEgreso && !esGasto) {
  //       // Solo sumamos si NO es egreso
  //       return acc + (Number(obj[propiedad]) || 0);
  //     }

  //     return acc;
  //   }, 0);
  // }

  getTotales(propiedad: string): number {
    if (!this.pagos || this.pagos.length === 0) return 0;

    // 1. Detectamos si el socio actual es el de "GASTOS" o "INGRESOS"
    // Puedes usar el ID (ej: '02') o el nombre.
    const esSocioEspecial = this.nombreSocio?.toUpperCase().includes('GASTOS');

    return this.pagos.reduce((acc, obj) => {
      const numAmort = String(obj.numamortizacion || '').toUpperCase();
      const esGastoOAyuda = numAmort.startsWith('GST') || numAmort.startsWith('AYUDA');

      if (esSocioEspecial) {
        // SI ES EL SOCIO GASTOS: Sumamos solo lo que sea GST o AYUDA
        return esGastoOAyuda ? acc + (Number(obj[propiedad]) || 0) : acc;
      } else {
        // SI ES UN SOCIO NORMAL: Sumamos solo lo que NO sea gasto/ayuda (lógica anterior)
        return !esGastoOAyuda ? acc + (Number(obj[propiedad]) || 0) : acc;
      }
    }, 0);
  }

  irASocio(): void {
    this.router.navigate(['/socios']);
  }

  botonDescargarPDF() {
    // Obtenemos el id directamente de la ruta activa
    const idSocio = this.route.snapshot.paramMap.get('id');

    if (idSocio && this.pagos.length > 0) {
      // Verificamos que los datos que tenemos pertenezcan al socio actual
      // (Asumiendo que tu objeto pago tiene la propiedad idsocio)
      this.generarPDFPagos(idSocio, this.pagos);
    } else {
      alert("No hay pagos cargados para este ID.");
    }
  }

}
