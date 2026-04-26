export class Prestamos {
    idprestamos!: number;
    tipo!: string;
    idsocio!: string
    nombresocio!: string;
    fechaprestamo!: Date;
    montoprestado!: number;
    plazoprestamo!: number;
    interesprestamo!: number;
    interesmensual!: number;
    interestotal!: number;
    amortizacion!: number;
    cuota!: number;
    total!: number;
    comentario!: string;

    // AGREGA ESTAS DOS:
    estatus_dinamico?: string;
    letras_resumen?: string;
}